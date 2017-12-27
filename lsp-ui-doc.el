;;; lsp-ui-doc.el --- Lsp-Ui-Doc  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui
;; Version: 0.0.1
;; Package-Requires: ((emacs "26") (lsp-mode "3.4") (markdown-mode "1.0"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Show documentation of the symbol at point in a child frame

;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'markdown-mode)

(defgroup lsp-ui-doc nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-doc) Top")
  :link '(info-link "(lsp-ui-doc) Customizing"))

(defcustom lsp-ui-doc-enable t
  "Whether or not to enable lsp-ui-doc."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-doc-header nil
  "Whether or not to enable the header which display the symbol string."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-position 'top
  "Where to display the doc."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-background "#031A25"
  "Background color of the frame.  To more customize the frame, see the varia..
ble `lsp-ui-doc-frame-parameters'"
  :type 'color
  :group 'lsp-ui-doc)

(defface lsp-ui-doc-header
  '((t :foreground "black"
       :background "deep sky blue"))
  "Face used on the header."
  :group 'lsp-ui-doc)

(defvar lsp-ui-doc-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 10)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (top . -1)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defvar-local lsp-ui-doc--bounds nil)

(declare-function lsp-ui-sideline--get-language 'lsp-ui-sideline)

(defmacro lsp-ui-doc--with-buffer (&rest body)
  "Execute BODY in the lsp-ui-doc buffer."
  `(with-current-buffer (get-buffer-create (lsp-ui-doc--make-buffer-name))
     (prog1 (let ((buffer-read-only nil))
              ,@body)
       (setq buffer-read-only t))))

(defmacro lsp-ui-doc--set-frame (frame)
  "Set the frame parameter 'lsp-ui-doc-frame to FRAME."
  `(set-frame-parameter nil 'lsp-ui-doc-frame ,frame))

(defmacro lsp-ui-doc--get-frame ()
  "Return the child frame."
  `(frame-parameter nil 'lsp-ui-doc-frame))

(defun lsp-ui-doc--make-buffer-name ()
  "Construct the buffer name, it should be unique for each frame."
  (concat "*lsp-ui-doc-"
          (or (frame-parameter nil 'window-id)
              (frame-parameter nil 'name))
          "*"))

(defun lsp-ui-doc--extract (contents)
  "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
  (when contents
    (cond
     ((stringp contents) contents)
     ((listp contents) ;; MarkedString[]
      (mapconcat
       (lambda (item) (if (stringp item) item (gethash "value" item)))
       (let ((language-id (lsp-ui-sideline--get-language)))
         (--remove-first
          (when (hash-table-p it)
            (let ((lang (gethash "language" it)))
              (or (string= language-id lang)
                  ;; A language server supporting language-id "cpp" may respond MarkedString{"language":"c"}
                  (and (string= language-id "cpp") (string= lang "c")))))
          contents))
       "\n\n"))
     ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
     ((gethash "language" contents) (gethash "value" contents)) ;; MarkedString
     )))

(defun lsp-ui-doc--make-request ()
  "Request the documentation to the LS."
  (when (bound-and-true-p lsp--cur-workspace)
    (if (symbol-at-point)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (unless (equal lsp-ui-doc--bounds bounds)
            (lsp--send-request-async (lsp--make-request "textDocument/hover"
                                                        (lsp--text-document-position-params))
                                     (lambda (hover)
                                       (lsp-ui-doc--callback hover bounds (current-buffer))
                                       ))))
      (lsp-ui-doc--hide-frame))))

(defun lsp-ui-doc--callback (hover bounds buffer)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made."
  (if (and hover
           (lsp--point-is-within-bounds-p (car bounds) (cdr bounds))
           (equal buffer (current-buffer)))
      (let ((doc (lsp-ui-doc--extract (gethash "contents" hover))))
        (setq lsp-ui-doc--bounds bounds)
        (lsp-ui-doc--display (thing-at-point 'symbol t) doc))
    (lsp-ui-doc--hide-frame)))

(defun lsp-ui-doc--hide-frame ()
  "Hide the frame."
  (setq lsp-ui-doc--bounds nil)
  (when (lsp-ui-doc--get-frame)
    (lsp-ui-doc--with-buffer
     (erase-buffer))
    (make-frame-invisible (lsp-ui-doc--get-frame))))

(defun lsp-ui-doc--buffer-width ()
  "Calcul the max width of the buffer."
  (lsp-ui-doc--with-buffer
   (save-excursion
     (let ((max 0))
       (goto-char (point-min))
       (while (not (eobp))
         (let* ((len (- (line-end-position) (line-beginning-position))))
           (when (> len max)
             (setq max len)))
         (forward-line 1))
       max))))

(defun lsp-ui-doc--line-height (&optional line)
  "Return the pos-y of the LINE on screen, in pixel."
  (nth 2 (or (window-line-height line)
             (and (redisplay t)
                  (window-line-height line)))))

(defun lsp-ui-doc--sideline-pos-y ()
  "."
  (-> (when (bound-and-true-p lsp-ui-sideline--occupied-lines)
        (-min lsp-ui-sideline--occupied-lines))
      (line-number-at-pos)
      (lsp-ui-doc--line-height)))

(defun lsp-ui-doc--resize-buffer ()
  "If the buffer's width is larger than the current window, resize it."
  (let* ((window-width (window-width))
         (fill-column (- window-width 5)))
    (when (> (lsp-ui-doc--buffer-width) window-width)
      (lsp-ui-doc--with-buffer
       (fill-region (point-min) (point-max))))))

(defun lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (lsp-ui-doc--resize-buffer)
  (fit-frame-to-buffer frame)
  (-let* (((_left top right _bottom) (window-edges nil nil t t))
          (c-width (frame-pixel-width))
          (c-height (frame-pixel-height))
          (mode-line-posy (lsp-ui-doc--line-height 'mode-line)))
    (set-frame-parameter frame 'top (pcase lsp-ui-doc-position
                                      ('top (+ top 10))
                                      ('bottom (- mode-line-posy c-height 10))))
    (set-frame-parameter frame 'left (- right c-width 10))))

(defun lsp-ui-doc--render-buffer (string symbol)
  "Set the BUFFER with STRING.
SYMBOL."
  (lsp-ui-doc--with-buffer
   (erase-buffer)
   (insert string)
   (goto-char (point-min))
   (markdown-view-mode)
   (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
   (setq-local window-min-height 1)
   (setq header-line-format (when lsp-ui-doc-header (concat " " symbol))
         mode-line-format nil
         cursor-type nil)))

(defun lsp-ui-doc--display (symbol string)
  "Display the documentation on screen.
SYMBOL STRING."
  (if (or (null string)
          (string-empty-p string))
      (lsp-ui-doc--hide-frame)
    (lsp-ui-doc--render-buffer string symbol)
    (unless (frame-live-p (lsp-ui-doc--get-frame))
      (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
    (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame))
    (unless (frame-visible-p (lsp-ui-doc--get-frame))
      (make-frame-visible (lsp-ui-doc--get-frame)))))

(defun lsp-ui-doc--make-frame ()
  "Create the child frame and return it."
  (let ((after-make-frame-functions nil)
        (before-make-frame-hook nil)
        (buffer (get-buffer (lsp-ui-doc--make-buffer-name)))
        (params (append lsp-ui-doc-frame-parameters
                        `((default-minibuffer-frame . ,(selected-frame))
                          (minibuffer . ,(minibuffer-window))
                          (background-color . ,lsp-ui-doc-background)))))
    (window-frame
     (display-buffer-in-child-frame
      buffer
      `((child-frame-parameters . ,params))))))

(defadvice select-window (after lsp-ui-doc--select-window activate)
  "Make powerline aware of window change."
  (lsp-ui-doc--hide-frame))

(define-minor-mode lsp-ui-doc-mode
  "Minor mode for showing hover information in child frame."
  :init-value nil
  :group lsp-ui-doc
  (if (< emacs-major-version 26)
      (message "lsp-ui-doc uses child frame which requires Emacs >= 26")
    (cond
     (lsp-ui-doc-mode
      (add-hook 'post-command-hook 'lsp-ui-doc--make-request nil t))
     (t
      (remove-hook 'post-command-hook 'lsp-ui-doc--make-request t)))))

(defun lsp-ui-doc-enable (enable)
  "ENABLE/disable lsp-ui-doc-mode.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-ui-doc-mode (if enable 1 -1)))

(provide 'lsp-ui-doc)
;;; lsp-ui-doc.el ends here
