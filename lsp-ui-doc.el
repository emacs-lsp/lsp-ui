;;; lsp-ui-doc.el --- Lsp-Ui-Doc  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui

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

(defcustom lsp-ui-doc-include-signature nil
  "Whether or not to include the object signature/type in the frame."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-position 'top
  "Where to display the doc."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "At point" at-point))
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-background "#272A36"
  "Background color of the frame.
To more customize the frame, see the variable
`lsp-ui-doc-frame-parameters'."
  :type 'color
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-border "white"
  "Border color of the frame."
  :type 'color
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-max-width 150
  "Maximum number of columns of the frame."
  :type 'integer
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-max-height 30
  "Maximum number of lines in the frame."
  :type 'integer
  :group 'lsp-ui-doc)

(defface lsp-ui-doc-header
  '((t :foreground "black"
       :background "deep sky blue"))
  "Face used on the header."
  :group 'lsp-ui-doc)

(defface lsp-ui-doc-url
  '((t :inherit link))
  "Face used on links."
  :group 'lsp-ui-doc)

(defvar lsp-ui-doc-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
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
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defvar lsp-ui-doc-render-function nil
  "Function called to format the documentation.
The function takes a string as parameter and should return a string.
If this variable is nil (the default), the documentation will be rendered
as markdown.")

(defvar lsp-ui-doc-custom-markup-modes
  '((rust-mode "no_run" "rust,no_run" "rust,ignore" "rust,should_panic"))
  "Mode to uses with markdown code blocks.
They are added to `markdown-code-lang-modes'")

(defvar lsp-ui-doc-frame-hook nil
  "Hooks run on child-frame creation.
The functions receive 2 parameters: the frame and its window.")

(defvar-local lsp-ui-doc--bounds nil)
(defvar-local lsp-ui-doc--string-eldoc nil)

(declare-function lsp-ui-sideline--get-renderer 'lsp-ui-sideline)

(defvar-local lsp-ui-doc--parent-vars nil
  "Variables from the parents frame that we want to access in the child.
Because some variables are buffer local.")

(defmacro lsp-ui-doc--with-buffer (&rest body)
  "Execute BODY in the lsp-ui-doc buffer."
  `(let ((parent-vars (list :buffer (current-buffer)
                            :window (get-buffer-window)
                            :workspace-root (when lsp--cur-workspace
                                              (lsp--workspace-root lsp--cur-workspace)))))
     (with-current-buffer (get-buffer-create (lsp-ui-doc--make-buffer-name))
       (setq lsp-ui-doc--parent-vars parent-vars)
       (prog1 (let ((buffer-read-only nil))
                ,@body)
         (setq buffer-read-only t)))))

(defmacro lsp-ui-doc--get-parent (var)
  "Return VAR in `lsp-ui-doc--parent-vars'."
  `(plist-get lsp-ui-doc--parent-vars ,var))

(defmacro lsp-ui-doc--set-frame (frame)
  "Set the frame parameter ‘lsp-ui-doc-frame’ to FRAME."
  `(set-frame-parameter nil 'lsp-ui-doc-frame ,frame))

(defmacro lsp-ui-doc--get-frame ()
  "Return the child frame."
  `(frame-parameter nil 'lsp-ui-doc-frame))

(defun lsp-ui-doc--make-buffer-name ()
  "Construct the buffer name, it should be unique for each frame."
  (concat " *lsp-ui-doc-"
          (or (frame-parameter nil 'window-id)
              (frame-parameter nil 'name))
          "*"))

(defun lsp-ui-doc--set-eldoc (marked-string)
  (when marked-string
    (let ((string (lsp-ui-doc--extract-marked-string marked-string)))
      (setq lsp-ui-doc--string-eldoc string))))

(defun lsp-ui-doc--eldoc (&rest _)
  (when (and (lsp--capability "documentHighlightProvider")
             lsp-highlight-symbol-at-point)
    (lsp-symbol-highlight))
  lsp-ui-doc--string-eldoc)

;; ‘markdown-fontify-code-block-default-mode’ isn’t yet available in
;; Markdown 2.3.
(defvar markdown-fontify-code-block-default-mode)

(defun lsp-ui-doc--setup-markdown (mode)
  "Setup the ‘markdown-mode’ in the frame.
MODE is the mode used in the parent frame."
  (make-local-variable 'markdown-code-lang-modes)
  (dolist (mark (alist-get mode lsp-ui-doc-custom-markup-modes))
    (add-to-list 'markdown-code-lang-modes (cons mark mode)))
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-fontify-code-block-default-mode mode)
  (setq-local markdown-hide-markup t))

(defun lsp-ui-doc--extract-marked-string (marked-string)
  "Render the MARKED-STRING."
  (string-trim-right
   (let* ((string (if (stringp marked-string)
                      marked-string
                    (gethash "value" marked-string)))
          (with-lang (hash-table-p marked-string))
          (language (and with-lang (gethash "language" marked-string)))
          (render-fn (if with-lang (lsp-ui-sideline--get-renderer language)
                       (and (functionp lsp-ui-doc-render-function)
                            lsp-ui-doc-render-function)))
          (mode major-mode))
     (if render-fn
         (funcall render-fn string)
       (with-temp-buffer
         (insert string)
         (delay-mode-hooks
           (funcall (cond ((and with-lang (string= "text" language)) 'text-mode)
                          ((fboundp 'gfm-view-mode) 'gfm-view-mode)
                          (t 'markdown-mode)))
           (when (derived-mode-p 'markdown-mode)
             (lsp-ui-doc--setup-markdown mode))
           (ignore-errors
             (font-lock-ensure)))
         (buffer-string))))))

(defun lsp-ui-doc--filter-marked-string (list-marked-string)
  (let ((groups (--separate (and (hash-table-p it)
                                 (lsp-ui-sideline--get-renderer (gethash "language" it)))
                            list-marked-string)))
    (lsp-ui-doc--set-eldoc (caar groups))
    (if lsp-ui-doc-include-signature
        list-marked-string
      (cadr groups))))

(defun lsp-ui-doc--extract (contents)
  "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
  (when contents
    (cond
     ((stringp contents) contents)
     ((listp contents) ;; MarkedString[]
      (mapconcat 'lsp-ui-doc--extract-marked-string
                 (lsp-ui-doc--filter-marked-string contents)
                 "\n\n"
                 ;; (propertize "\n\n" 'face '(:height 0.4))
                 ))
     ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
     ((gethash "language" contents) ;; MarkedString
      (lsp-ui-doc--extract-marked-string contents)))))

(defun lsp-ui-doc--make-request ()
  "Request the documentation to the LS."
  (when (and (bound-and-true-p lsp--cur-workspace)
             (not (bound-and-true-p lsp-ui-peek-mode)))
    (if (symbol-at-point)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (unless (equal lsp-ui-doc--bounds bounds)
            (lsp--send-request-async (lsp--make-request "textDocument/hover"
                                                        (lsp--text-document-position-params))
                                     (lambda (hover)
                                       (lsp-ui-doc--callback hover bounds (current-buffer))
                                       ))))
      (setq lsp-ui-doc--string-eldoc nil)
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
    (setq lsp-ui-doc--string-eldoc nil)
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
  (-> (when (bound-and-true-p lsp-ui-sideline--occupied-lines)
        (-min lsp-ui-sideline--occupied-lines))
      (line-number-at-pos)
      (lsp-ui-doc--line-height)))

(defun lsp-ui-doc--resize-buffer ()
  "If the buffer's width is larger than the current window, resize it."
  (let* ((window-width (window-width))
         (fill-column (min lsp-ui-doc-max-width (- window-width 5))))
    (when (> (lsp-ui-doc--buffer-width) (min lsp-ui-doc-max-width window-width))
      (lsp-ui-doc--with-buffer
       (fill-region (point-min) (point-max))))))

(defun lsp-ui-doc--mv-at-point (frame height start-x start-y)
  "Move the FRAME at point.
HEIGHT is the child frame height.
START-X is the position x of the current window.
START-Y is the position y of the current window."
  (-let* (((x . y) (--> (bounds-of-thing-at-point 'symbol)
                        (nth 2 (posn-at-point (car it)))))
          (mode-line-y (lsp-ui-doc--line-height 'mode-line))
          (char-height (frame-char-height))
          (y (or (and (> y (/ mode-line-y 2))
                      (<= (- mode-line-y y) (+ char-height height))
                      (> (- y height) 0)
                      (- y height))
                 (+ y char-height))))
    (set-frame-position frame (+ x start-x) (+ y start-y))))

(defun lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (lsp-ui-doc--resize-buffer)
  (-let* (((left top right _bottom) (window-edges nil nil nil t))
          (window (frame-root-window frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000))
          (width (+ width (* (frame-char-width frame) 2))) ;; margins
          (char-h (frame-char-height))
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (frame-resize-pixelwise t))
    (set-window-margins window 1 1)
    (set-frame-size frame width height t)
    (if (eq lsp-ui-doc-position 'at-point)
        (lsp-ui-doc--mv-at-point frame height left top)
      (set-frame-position frame (- right width 10)
                          (pcase lsp-ui-doc-position
                            ('top (+ top 10))
                            ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                                        height
                                        10)))))))

(defun lsp-ui-doc--visit-file (filename)
  "Visit FILENAME in the parent frame."
  (-some->> (find-file-noselect filename)
            (set-window-buffer (lsp-ui-doc--get-parent :window))))

(defun lsp-ui-doc--put-click (bounds fn)
  "Add text properties on text to make it clickable.
The text delimiters are BOUNDS.
FN is the function to call on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] fn)
    (put-text-property (car bounds) (cdr bounds) 'keymap map)
    (put-text-property (car bounds) (cdr bounds) 'mouse-face
                       (list :inherit 'lsp-ui-doc-url
                             :box (list :line-width -1
                                        :color (face-foreground 'lsp-ui-doc-url))))
    (add-face-text-property (car bounds) (cdr bounds) 'lsp-ui-doc-url)))

(defun lsp-ui-doc--make-clickable-link ()
  "Find paths and urls in the buffer and make them clickable."
  (goto-char (point-min))
  (save-excursion
    (while (not (eobp))
      ;;; TODO:
      ;;;  Search path in the whole buffer.
      ;;;  For now, it searches only on beginning of lines.
      (-when-let* ((filename (thing-at-point 'filename))
                   (path (if (file-readable-p filename) filename
                           (let ((full (concat (lsp-ui-doc--get-parent :workspace-root)
                                               filename)))
                             (and (file-readable-p full)
                                  full)))))
        (lsp-ui-doc--put-click (bounds-of-thing-at-point 'filename)
                               (lambda () (interactive)
                                 (lsp-ui-doc--visit-file path))))
      (forward-line 1))
    (goto-char (point-min))
    (condition-case nil
        (while (search-forward-regexp "http[s]?://")
          (lsp-ui-doc--put-click (thing-at-point-bounds-of-url-at-point)
                                 'browse-url-at-mouse))
      (search-failed nil))))

(defun lsp-ui-doc--render-buffer (string symbol)
  "Set the buffer with STRING."
  (lsp-ui-doc--with-buffer
   (erase-buffer)
   (insert (concat (propertize "\n" 'face '(:height 0.2))
                   string
                   (propertize "\n\n" 'face '(:height 0.3))))
   (lsp-ui-doc--make-clickable-link)
   (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
   (setq-local window-min-height 1)
   (setq header-line-format (when lsp-ui-doc-header (concat " " symbol))
         mode-line-format nil
         cursor-type nil)))

(defun lsp-ui-doc--display (symbol string)
  "Display the documentation on screen."
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
  (lsp-ui-doc--delete-frame)
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (name-buffer (lsp-ui-doc--make-buffer-name))
         (buffer (get-buffer name-buffer))
         (params (append lsp-ui-doc-frame-parameters
                         `((default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (background-color . ,lsp-ui-doc-background))))
         (window (display-buffer-in-child-frame
                  buffer
                  `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (set-frame-parameter nil 'lsp-ui-doc-buffer buffer)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-background 'internal-border lsp-ui-doc-border frame)
    (run-hook-with-args 'lsp-ui-doc-frame-hook frame window)
    frame))

(defun lsp-ui-doc--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (lsp-ui-doc--get-frame))
    (delete-frame frame)
    (lsp-ui-doc--set-frame nil)))

(defadvice select-window (after lsp-ui-doc--select-window activate)
  "Delete the child frame if window changes."
  (lsp-ui-doc--hide-frame))

(defadvice load-theme (after lsp-ui-doc--delete-frame-on-theme-load activate)
  "Force a frame refresh on theme reload."
  (lsp-ui-doc--delete-frame))

(defun lsp-ui-doc-enable-eldoc ()
  (setq-local eldoc-documentation-function 'lsp-ui-doc--eldoc))

(defun lsp-ui-doc--on-delete (frame)
  "Function called when a FRAME is deleted."
  (-some--> (frame-parameter frame 'lsp-ui-doc-buffer)
            (get-buffer it)
            (and (buffer-live-p it) it)
            (kill-buffer it)))

(define-minor-mode lsp-ui-doc-mode
  "Minor mode for showing hover information in child frame."
  :init-value nil
  :group lsp-ui-doc
  (if (< emacs-major-version 26)
      (message "lsp-ui-doc uses child frame which requires Emacs >= 26")
    (cond
     (lsp-ui-doc-mode
      (progn
        (with-eval-after-load 'frameset
          ;; The documentation frame can’t be properly restored.  Especially
          ;; ‘desktop-save’ will misbehave and save a bogus string "Unprintable
          ;; entity" in the desktop file.  Therefore we have to prevent
          ;; ‘frameset-save’ from saving the parameter.
          (unless (assq 'lsp-ui-doc-frame frameset-filter-alist)
            ;; Copy the variable first.  See the documentation of
            ;; ‘frameset-filter-alist’ for explanation.
            (cl-callf copy-tree frameset-filter-alist)
            (push '(lsp-ui-doc-frame . :never) frameset-filter-alist)))
        (add-hook 'lsp-after-open-hook 'lsp-ui-doc-enable-eldoc nil t)
        (add-hook 'post-command-hook 'lsp-ui-doc--make-request nil t)
        (add-hook 'delete-frame-functions 'lsp-ui-doc--on-delete nil t)))
     (t
      (remove-hook 'delete-frame-functions 'lsp-ui-doc--on-delete t)
      (remove-hook 'post-command-hook 'lsp-ui-doc--make-request t)
      (remove-hook 'lsp-after-open-hook 'lsp-ui-doc-enable-eldoc t)
      (setq-local eldoc-documentation-function 'lsp--on-hover)))))

(defun lsp-ui-doc-enable (enable)
  "Enable/disable ‘lsp-ui-doc-mode’.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-ui-doc-mode (if enable 1 -1)))

(provide 'lsp-ui-doc)
;;; lsp-ui-doc.el ends here
