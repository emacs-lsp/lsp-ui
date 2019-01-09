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
(require 'dash-functional)
(require 'goto-addr)
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

(defcustom lsp-ui-doc-use-childframe t
  "Whether to display documentation in a child-frame or the current frame.
Child frames requires GNU/Emacs version >= 26 and graphical frames."
  :type 'boolean
  :group 'lsp-ui-doc)

(defface lsp-ui-doc-background
  '((((background light)) :background "#b3b3b3")
    (t :background "#272A36"))
  "Background color of the documentation.
Only the `background' is used in this face."
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
    (no-special-glyphs . t)
    (desktop-dont-save . t))
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

;; Avoid warning with emacs < 26
(declare-function display-buffer-in-child-frame "window.el")

(defvar-local lsp-ui-doc--parent-vars nil
  "Variables from the parents frame that we want to access in the child.
Because some variables are buffer local.")

(defvar-local lsp-ui-doc--inline-ov nil
  "Overlay used to display the documentation in the buffer.")

(defmacro lsp-ui-doc--with-buffer (&rest body)
  "Execute BODY in the lsp-ui-doc buffer."
  `(let ((parent-vars (list :buffer (current-buffer)
                            :window (get-buffer-window)
                            :workspace-root (lsp-workspace-root))))
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

(defun lsp-ui-doc--get-frame (&optional include-deleted-frame)
  "Return the child frame."
  (let ((frame (frame-parameter nil 'lsp-ui-doc-frame)))
    (and (frame-live-p frame) frame)))

(defun lsp-ui-doc--make-buffer-name ()
  "Construct the buffer name, it should be unique for each frame."
  (concat " *lsp-ui-doc-"
          (or (frame-parameter nil 'window-id)
              (frame-parameter nil 'name))
          "*"))

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

(defun lsp-ui-doc--inline-wrapped-line (string)
  "Wraps a line of text for inline display."
  (cond ((string-empty-p string) "")
        ((< (length string) lsp-ui-doc-max-width) string)
        (t (concat (substring string 0 (- lsp-ui-doc-max-width 1))
                   "\\\n"
                   (lsp-ui-doc--inline-wrapped-line (substring string (- lsp-ui-doc-max-width 1)))))))

(defun lsp-ui-doc--inline-formatted-string (string)
  "Formats STRING for inline rendering."
  (mapconcat (lambda (line)
               (lsp-ui-doc--inline-wrapped-line (string-trim-right line)))
             (split-string string "\n")
             "\n"))

(defun lsp-ui-doc--extract-marked-string (marked-string)
  "Render the MARKED-STRING."
  (string-trim-right
   (let* ((string (if (stringp marked-string)
                      marked-string
                    (gethash "value" marked-string)))
          (with-lang (hash-table-p marked-string))
          (language (and with-lang (gethash "language" marked-string)))
          (render-fn (if with-lang (lsp-get-renderer language)
                       (and (functionp lsp-ui-doc-render-function)
                            lsp-ui-doc-render-function)))
          (mode major-mode))
     (if render-fn
         (funcall render-fn string)
       (with-temp-buffer
         (if (lsp-ui-doc--inline-p)
             (insert (lsp-ui-doc--inline-formatted-string string))
           (insert string))

         (delay-mode-hooks
           (let ((inhibit-message t))
             (funcall (cond ((and with-lang (string= "text" language)) 'text-mode)
                            ((fboundp 'gfm-view-mode) 'gfm-view-mode)
                            (t 'markdown-mode))))
           (when (derived-mode-p 'markdown-mode)
             (lsp-ui-doc--setup-markdown mode))
           (ignore-errors
             (font-lock-ensure)))
         (buffer-string))))))

(defun lsp-ui-doc--filter-marked-string (list-marked-string)
  (let ((groups (--separate (and (hash-table-p it)
                                 (lsp-get-renderer (gethash "language" it)))
                            (append list-marked-string nil))))
    (if lsp-ui-doc-include-signature
        list-marked-string
      (cadr groups))))

(defun lsp-ui-doc--extract (contents)
  "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
  (cond
   ((stringp contents) contents)
   ((sequencep contents) ;; MarkedString[]
    (mapconcat 'lsp-ui-doc--extract-marked-string
               (lsp-ui-doc--filter-marked-string contents)
               "\n\n"
               ;; (propertize "\n\n" 'face '(:height 0.4))
               ))
   ;; when we get markdown contents, render using emacs gfm-view-mode / markdown-mode
   ((string= (gethash "kind" contents) "markdown") (lsp-ui-doc--extract-marked-string contents))
   ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
   ((gethash "language" contents) ;; MarkedString
    (lsp-ui-doc--extract-marked-string contents))))

(defun lsp-ui-doc--hide-frame ()
  "Hide the frame."
  (when (overlayp lsp-ui-doc--inline-ov)
    (delete-overlay lsp-ui-doc--inline-ov))
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
  "If the buffer's width is larger than the current frame, resize it."
  (let* ((frame-width (frame-width))
         (fill-column (min lsp-ui-doc-max-width (- frame-width 5))))
    (when (> (lsp-ui-doc--buffer-width) (min lsp-ui-doc-max-width frame-width))
      (lsp-ui-doc--with-buffer
       (fill-region (point-min) (point-max))))))

(defun lsp-ui-doc--next-to-side-window-p nil
  "Return non-nil if the window on the left is a side window."
  (let* ((win (window-at 0 0))
         (left (window-left (selected-window))))
    (and (not (eq win (selected-window)))
         (or (not left) (eq win left))
         (eq (window-parameter win 'window-side) 'left))))

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
  (-let* (((left top _right _bottom) (window-edges nil nil nil t))
          (window (frame-root-window frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
          (width (+ width (* (frame-char-width frame) 1))) ;; margins
          (char-h (frame-char-height))
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (frame-resize-pixelwise t))
    (set-frame-size frame width height t)
    (if (eq lsp-ui-doc-position 'at-point)
        (lsp-ui-doc--mv-at-point frame height left top)
      (set-frame-position frame
                          (if (and (>= left (+ width 10 (frame-char-width)))
                                   (not (lsp-ui-doc--next-to-side-window-p)))
                              10
                            (- (frame-pixel-width) width 10 (frame-char-width)))
                          (pcase lsp-ui-doc-position
                            ('top (+ top 10))
                            ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                                        height
                                        10)))))))

(defun lsp-ui-doc--visit-file (filename)
  "Visit FILENAME in the parent frame."
  (-some->> (find-file-noselect filename)
            (set-window-buffer (lsp-ui-doc--get-parent :window))))

(defun lsp-ui-doc--put-click (start end fn)
  "Add text properties on text to make it clickable.
The text delimiters are BOUNDS.
FN is the function to call on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] fn)
    (put-text-property start end 'keymap map)
    (put-text-property start end 'mouse-face
                       (list :inherit 'lsp-ui-doc-url
                             :box (list :line-width -1
                                        :color (face-foreground 'lsp-ui-doc-url))))
    (add-face-text-property start end 'lsp-ui-doc-url)))

(defun lsp-ui-doc--make-clickable-link ()
  "Find paths and urls in the buffer and make them clickable."
  (goto-char (point-min))
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search)
      (while (re-search-forward goto-address-url-regexp nil t)
        (goto-char (1+ (match-end 0)))
        (lsp-ui-doc--put-click (match-beginning 0) (match-end 0)
                               'browse-url-at-mouse)))))

(defun lsp-ui-doc--render-buffer (string symbol)
  "Set the buffer with STRING."
  (lsp-ui-doc--with-buffer
   (erase-buffer)
   (let ((inline-p (lsp-ui-doc--inline-p)))
     (insert (concat (unless inline-p (propertize "\n" 'face '(:height 0.2)))
                     (-> (replace-regexp-in-string "`\\([\n]+\\)" "" string)
                         (string-trim-right))
                     (unless inline-p (propertize "\n\n" 'face '(:height 0.3))))))
   (lsp-ui-doc--make-clickable-link)
   (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
   (setq-local window-min-height 1)
   (setq header-line-format (when lsp-ui-doc-header (concat " " symbol))
         mode-line-format nil
         cursor-type nil)))

(defun lsp-ui-doc--inline-height ()
  (lsp-ui-doc--with-buffer
   (length (split-string (buffer-string) "\n"))))

(defun lsp-ui-doc--remove-invisibles (string)
  "Remove invisible characters in STRING."
  (let* ((start (text-property-not-all 0 (length string) 'invisible nil string)))
    (while start
      (setq string (concat (substring string 0 start)
                           (-some->> (next-single-property-change start 'invisible string)
                                     (substring string))))
      (setq start (text-property-not-all 0 (length string) 'invisible nil string)))
    string))

(defvar-local lsp-ui-doc--inline-width nil)

(defun lsp-ui-doc--inline-window-width nil
  (- (min (window-text-width)
          (window-body-width))
     (if (bound-and-true-p display-line-numbers-mode)
         (+ 2 (line-number-display-width))
       0)
     1))

(defun lsp-ui-doc--inline-zip (s1 s2)
  (let* ((width (lsp-ui-doc--inline-window-width))
         (max-s1 (- width lsp-ui-doc--inline-width 2)))
    (truncate-string-to-width
     (concat (truncate-string-to-width s1 max-s1 nil ?\s) s2)
     width nil ?\s)))

(defun lsp-ui-doc--inline-padding (string len)
  (let ((string (concat " " string (make-string (- len (string-width string)) ?\s) " ")))
    (add-face-text-property 0 (length string) (list :background (face-background 'lsp-ui-doc-background nil t)) t string)
    string))

(defun lsp-ui-doc--inline-faking-frame (doc-strings)
  (let* ((len-max (-max-by '> (-map 'string-width doc-strings))))
    (setq lsp-ui-doc--inline-width len-max)
    (--map (lsp-ui-doc--inline-padding it len-max) doc-strings)))

(defun lsp-ui-doc--inline-untab (string)
  (replace-regexp-in-string "\t" (make-string tab-width ?\s) string nil t))

(defun lsp-ui-doc--inline-merge (strings)
  (let* ((buffer-strings (-> (lsp-ui-doc--inline-untab strings)
                             (lsp-ui-doc--remove-invisibles)
                             (split-string "\n")))
         (doc-strings (-> (lsp-ui-doc--with-buffer (buffer-string))
                          (lsp-ui-doc--inline-untab)
                          (lsp-ui-doc--remove-invisibles)
                          (split-string "\n")))
         (merged (--> (lsp-ui-doc--inline-faking-frame doc-strings)
                      (-zip-with 'lsp-ui-doc--inline-zip buffer-strings it)
                      (string-join it "\n")
                      (concat it "\n"))))
    (add-face-text-property 0 (length merged) 'default t merged)
    merged))

(defun lsp-ui-doc--inline-pos-at (start lines)
  "Calcul the position at START + forward n LINES."
  (save-excursion (goto-char start)
                  (forward-line lines)
                  (point)))

(defun lsp-ui-doc--inline-pos (height)
  "Return a cons of positions where to place the doc.
HEIGHT is the documentation number of lines."
  (let* ((w-start (window-start))
         (w-end (lsp-ui-doc--inline-pos-at w-start (window-body-height)))
         (ov-end (lsp-ui-doc--inline-pos-at w-start height)))
    (cond
     ;; Display on top ?
     ((< (lsp-ui-doc--inline-pos-at ov-end 1) (point))
      (cons w-start ov-end))
     ;; Display at the bottom ?
     ((>= (lsp-ui-doc--inline-pos-at w-end (- height))
          (lsp-ui-doc--inline-pos-at (point) 2))
      (cons (lsp-ui-doc--inline-pos-at w-end (- height))
            w-end))
     ;; The doc is too long to display it fixed to the bottom ?
     ;; Then display 2 lines after `point'
     ;; The end of the documentation won't be visible in the window
     (t (cons (lsp-ui-doc--inline-pos-at (point) 2)
              (lsp-ui-doc--inline-pos-at (point) (+ height 2)))))))

(defun lsp-ui-doc--inline ()
  "Display the doc in the buffer."
  (-let* ((height (lsp-ui-doc--inline-height))
          ((start . end) (lsp-ui-doc--inline-pos height))
          (buffer-string (buffer-substring start end))
          (ov (if (overlayp lsp-ui-doc--inline-ov) lsp-ui-doc--inline-ov
                (setq lsp-ui-doc--inline-ov (make-overlay start end)))))
    (move-overlay ov start end)
    (overlay-put ov 'display (lsp-ui-doc--inline-merge buffer-string))
    (overlay-put ov 'lsp-ui-doc-inline t)
    (overlay-put ov 'window (selected-window))))

(defun lsp-ui-doc--inline-p ()
  "Return non-nil when the documentation should be display without a child frame."
  (or (not lsp-ui-doc-use-childframe)
      (not (display-graphic-p))
      (not (fboundp 'display-buffer-in-child-frame))))

(defun lsp-ui-doc--display (symbol string)
  "Display the documentation."
  (if (or (null string) (string-empty-p string))
      (lsp-ui-doc--hide-frame)
    (lsp-ui-doc--render-buffer string symbol)
    (if (lsp-ui-doc--inline-p)
        (lsp-ui-doc--inline)
      (unless (lsp-ui-doc--get-frame)
        (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
      (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame))
      (unless (frame-visible-p (lsp-ui-doc--get-frame))
        (make-frame-visible (lsp-ui-doc--get-frame))))))

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
                           (left-fringe . ,(frame-char-width))
                           (background-color . ,(face-background 'lsp-ui-doc-background nil t)))))
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
  (unless (equal (ad-get-arg 0) (selected-window))
    (lsp-ui-doc--hide-frame)))

(advice-add 'load-theme :before (lambda (&rest _) (lsp-ui-doc--delete-frame)))
(add-hook 'window-configuration-change-hook #'lsp-ui-doc--hide-frame)

(defun lsp-ui-doc--on-delete (frame)
  "Function called when a FRAME is deleted."
  (-some--> (frame-parameter frame 'lsp-ui-doc-buffer)
            (get-buffer it)
            (and (buffer-live-p it) it)
            (kill-buffer it)))

(defun lsp-ui-doc--on-hover (hover)
  "Handler for `lsp-on-hover-hook'.
HOVER is the returned signature information."
  (--if-let (-some->> hover (gethash "contents"))
      (lsp-ui-doc--display (thing-at-point 'symbol t)
                           (lsp-ui-doc--extract it))
    (eldoc-message nil)
    (lsp-ui-doc--hide-frame)))

(define-minor-mode lsp-ui-doc-mode
  "Minor mode for showing hover information in child frame."
  :init-value nil
  :group lsp-ui-doc
  (cond
   (lsp-ui-doc-mode
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

    (add-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover nil t)
    (add-hook 'delete-frame-functions 'lsp-ui-doc--on-delete nil t))
   (t
    (remove-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover t)
    (remove-hook 'delete-frame-functions 'lsp-ui-doc--on-delete t))))

(defun lsp-ui-doc-enable (enable)
  "Enable/disable ‘lsp-ui-doc-mode’.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-ui-doc-mode (if enable 1 -1)))

(defun lsp-ui-doc-show ()
  "Trigger display hover information popup."
  (interactive)
  (lsp-ui-doc--on-hover (lsp-request "textDocument/hover" (lsp--text-document-position-params))))

(defun lsp-ui-doc-hide ()
  "Hide hover information popup."
  (interactive)
  (lsp-ui-doc--hide-frame))

(provide 'lsp-ui-doc)
;;; lsp-ui-doc.el ends here
