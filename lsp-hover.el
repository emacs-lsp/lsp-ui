;;; lsp-hover.el --- Lsp-Hover  -*- lexical-binding: t -*-

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

(defgroup lsp-hover nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-hover) Top")
  :link '(info-link "(lsp-hover) Customizing"))

(defcustom lsp-hover-enable t
  "Whether or not to enable lsp-hover."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-hover-header nil
  "Whether or not to enable the header which display the symbol string."
  :type 'boolean
  :group 'lsp-hover)

(defcustom lsp-hover-position 'top
  "Where to display the doc."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'lsp-hover)

(defcustom lsp-hover-background "#031A25"
  "Background color of the frame.  To more customize the frame, see the varia..
ble `lsp-hover-frame-parameters'"
  :type 'color
  :group 'lsp-hover)

(defface lsp-hover-header
  '((t :foreground "black"
       :background "deep sky blue"))
  "Face used on the header."
  :group 'lsp-hover)

(defvar lsp-hover-frame-parameters
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

(defvar-local lsp-hover--bounds nil)

(declare-function lsp-line--get-language 'lsp-line)

(defmacro lsp-hover--with-buffer (&rest body)
  "Execute BODY in the lsp-hover buffer."
  `(with-current-buffer (get-buffer-create (lsp-hover--make-buffer-name))
     (prog1 (let ((buffer-read-only nil))
              ,@body)
       (setq buffer-read-only t))))

(defmacro lsp-hover--set-frame (frame)
  "Set the frame parameter 'lsp-hover-frame to FRAME."
  `(set-frame-parameter nil 'lsp-hover-frame ,frame))

(defmacro lsp-hover--get-frame ()
  "Return the child frame."
  `(frame-parameter nil 'lsp-hover-frame))

(defun lsp-hover--make-buffer-name ()
  "Construct the buffer name, it should be unique for each frame."
  (concat "*lsp-hover-"
          (or (frame-parameter nil 'window-id)
              (frame-parameter nil 'name))
          "*"))

(defun lsp-hover--extract (contents)
  "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
  (when contents
    (cond
     ((stringp contents) contents)
     ((listp contents) ;; MarkedString[]
      (mapconcat (lambda (item) (if (stringp item) item (gethash "value" item)))
                 (--remove-first (when (hash-table-p it)
                                   (string= (gethash "language" it)
                                            (lsp-line--get-language)))
                                 contents)
                 "\n\n"))
     ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
     ((gethash "language" contents) (gethash "value" contents)) ;; MarkedString
     )))

(defun lsp-hover--make-request ()
  "Request the documentation to the LS."
  (when (bound-and-true-p lsp--cur-workspace)
    (if (symbol-at-point)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (unless (equal lsp-hover--bounds bounds)
            (lsp--send-request-async (lsp--make-request "textDocument/hover"
                                                        (lsp--text-document-position-params))
                                     (lambda (hover)
                                       (lsp-hover--callback hover bounds (current-buffer))
                                       ))))
      (lsp-hover--hide-frame))))

(defun lsp-hover--callback (hover bounds buffer)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made."
  (if (and hover
           (lsp--point-is-within-bounds-p (car bounds) (cdr bounds))
           (equal buffer (current-buffer)))
      (let ((doc (lsp-hover--extract (gethash "contents" hover))))
        (setq lsp-hover--bounds bounds)
        (lsp-hover--display (thing-at-point 'symbol t) doc))
    (lsp-hover--hide-frame)))

(defun lsp-hover--hide-frame ()
  "Hide the frame."
  (setq lsp-hover--bounds nil)
  (when (lsp-hover--get-frame)
    (lsp-hover--with-buffer
     (erase-buffer))
    (make-frame-invisible (lsp-hover--get-frame))))

(defun lsp-hover--buffer-width ()
  "Calcul the max width of the buffer."
  (lsp-hover--with-buffer
   (save-excursion
     (let ((max 0))
       (goto-char (point-min))
       (while (not (eobp))
         (let* ((len (- (line-end-position) (line-beginning-position))))
           (when (> len max)
             (setq max len)))
         (forward-line 1))
       max))))

(defun lsp-hover--line-height (&optional line)
  "Return the pos-y of the LINE on screen, in pixel."
  (nth 2 (or (window-line-height line)
             (and (redisplay t)
                  (window-line-height line)))))

(defun lsp-hover--sideline-pos-y ()
  "."
  (-> (when (bound-and-true-p lsp-line--occupied-lines)
        (-min lsp-line--occupied-lines))
      (line-number-at-pos)
      (lsp-hover--line-height)))

(defun lsp-hover--resize-buffer ()
  "If the buffer's width is larger than the current window, resize it."
  (let* ((window-width (window-width))
         (fill-column (- window-width 5)))
    (when (> (lsp-hover--buffer-width) window-width)
      (lsp-hover--with-buffer
       (fill-region (point-min) (point-max))))))

(defun lsp-hover--move-frame (frame)
  "Place our FRAME on screen."
  (lsp-hover--resize-buffer)
  (-let* (((_left top right _bottom) (window-edges nil nil t t))
          ((&alist 'outer-size child-frame-size) (frame-geometry frame))
          ((c-width . c-height) child-frame-size)
          (mode-line-posy (lsp-hover--line-height 'mode-line)))
    (set-frame-parameter frame 'top (pcase lsp-hover-position
                                      ('top (+ top 10))
                                      ('bottom (- mode-line-posy c-height 10))))
    (set-frame-parameter frame 'left (- right c-width 10))
    (fit-frame-to-buffer frame)))

(defun lsp-hover--render-buffer (string symbol)
  "Set the BUFFER with STRING.
SYMBOL."
  (lsp-hover--with-buffer
   (erase-buffer)
   (insert string)
   (goto-char (point-min))
   (markdown-view-mode)
   (setq-local face-remapping-alist `((header-line lsp-hover-header)))
   (setq-local window-min-height 1)
   (setq header-line-format (when lsp-hover-header (concat " " symbol))
         mode-line-format nil
         cursor-type nil)))

(defun lsp-hover--display (symbol string)
  "Display the documentation on screen.
SYMBOL STRING."
  (if (or (null string)
          (string-empty-p string))
      (lsp-hover--hide-frame)
    (lsp-hover--render-buffer string symbol)
    (if (not (frame-live-p (lsp-hover--get-frame)))
        (lsp-hover--set-frame (lsp-hover--make-frame))
      (unless (frame-visible-p (lsp-hover--get-frame))
        (make-frame-visible (lsp-hover--get-frame))))
    (lsp-hover--move-frame (lsp-hover--get-frame))))

(defun lsp-hover--make-frame ()
  "Create the child frame and return it."
  (let ((after-make-frame-functions nil)
        (before-make-frame-hook nil)
        (buffer (get-buffer (lsp-hover--make-buffer-name)))
        (params (append lsp-hover-frame-parameters
                        `((default-minibuffer-frame . ,(selected-frame))
                          (minibuffer . ,(minibuffer-window))
                          (background-color . ,lsp-hover-background)))))
    (window-frame
     (display-buffer-in-child-frame
      buffer
      `((child-frame-parameters . ,params))))))

(defadvice select-window (after lsp-hover--select-window activate)
  "Make powerline aware of window change."
  (lsp-hover--hide-frame))

(define-minor-mode lsp-hover-mode
  "Minor mode for showing hover information in child frame."
  :init-value nil
  :group lsp-hover
  (if (< emacs-major-version 26)
      (message "lsp-hover uses child frame which requires Emacs >= 26")
    (cond
     (lsp-hover-mode
      (add-hook 'post-command-hook 'lsp-hover--make-request nil t))
     (t
      (remove-hook 'post-command-hook 'lsp-hover--make-request t)))))

(defun lsp-hover-enable (enable)
  "ENABLE/disable lsp-hover-mode.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-hover-mode (if enable 1 -1)))

(provide 'lsp-hover)
;;; lsp-hover.el ends here
