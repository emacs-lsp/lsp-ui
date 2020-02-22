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
(require 'cl-lib)

(when (featurep 'xwidget-internal)
  (require 'xwidget))

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

(defcustom lsp-ui-doc-position 'at-point
  "Where to display the doc."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "At point" at-point))
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-alignment 'frame
  "How to align the doc.
This only takes effect when `lsp-ui-doc-position' is 'top or 'bottom."
  :type '(choice (const :tag "Frame" frame)
                 (const :tag "Window" window))
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

(defcustom lsp-ui-doc-use-webkit nil
  "Whether to display documentation in a WebKit widget in a child-frame.
This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets`
option."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-delay 0.2
  "Number of seconds before showing the doc."
  :type 'number
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-winum-ignore t
  "Whether to ignore lsp-ui-doc buffers in winum."
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

(defvar lsp-ui-doc-frame-hook nil
  "Hooks run on child-frame creation.
The functions receive 2 parameters: the frame and its window.")

(defvar lsp-ui-doc-webkit-client-path
  (concat "file://"
          (file-name-directory (or load-file-name buffer-file-name))
          "lsp-ui-doc.html")
  "Path to the page loaded when a WebKit widget is created.")

;; Avoid warning with emacs < 26
(declare-function display-buffer-in-child-frame "window.el")

(defvar-local lsp-ui-doc--parent-vars nil
  "Variables from the parents frame that we want to access in the child.
Because some variables are buffer local.")

(defvar-local lsp-ui-doc--inline-ov nil
  "Overlay used to display the documentation in the buffer.")

(defvar-local lsp-ui-doc--bounds nil)
(defvar-local lsp-ui-doc--timer nil)

(defconst lsp-ui-doc--buffer-prefix " *lsp-ui-doc-")

(defmacro lsp-ui-doc--with-buffer (&rest body)
  "Execute BODY in the lsp-ui-doc buffer."
  `(let ((parent-vars (list :buffer (current-buffer)
                            :window (get-buffer-window))))
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

(defun lsp-ui-doc--get-frame (&optional _)
  "Return the child frame."
  (let ((frame (frame-parameter nil 'lsp-ui-doc-frame)))
    (and (frame-live-p frame) frame)))

(defsubst lsp-ui-doc--frame-visible-p ()
  "Return child frame visibility."
  (let ((frame (lsp-ui-doc--get-frame)))
    (and frame (frame-visible-p frame))))

(defun lsp-ui-doc--make-buffer-name ()
  "Construct the buffer name, it should be unique for each frame."
  (concat lsp-ui-doc--buffer-prefix
          (or (frame-parameter nil 'window-id)
              (frame-parameter nil 'name))
          "*"))

;; ‘markdown-fontify-code-block-default-mode’ isn’t yet available in
;; Markdown 2.3.
(defvar markdown-fontify-code-block-default-mode)

(defun lsp-ui-doc--inline-wrapped-line (string)
  "Wraps a line of text for inline display."
  (let ((doc-max-width (lsp-ui-doc--inline-window-width)))
    (cond ((string-empty-p string) "")
          ((< (length string) doc-max-width) string)
          (t (concat (substring string 0 (- doc-max-width 4))
                     "\n"
                     (string-trim-left
                      (lsp-ui-doc--inline-wrapped-line
                       (substring string (- doc-max-width 4)))))))))

(defun lsp-ui-doc--inline-formatted-string (string)
  "Formats STRING for inline rendering."
  (mapconcat (lambda (line)
               (lsp-ui-doc--inline-wrapped-line (string-trim-right line)))
             (split-string string "[\n\v\f\r]")
             "\n"))

(defun lsp-ui-doc--extract-marked-string (marked-string &optional language)
  "Render the MARKED-STRING."
  (string-trim-right
   (let* ((string (if (stringp marked-string)
                      marked-string
                    (gethash "value" marked-string)))
          (with-lang (hash-table-p marked-string))
          (language (or (and with-lang (or (gethash "language" marked-string) (gethash "kind" marked-string)))
                        language)))
     (cond
      (lsp-ui-doc-use-webkit
       (if (and language (not (string= "text" language)))
           (format "```%s\n%s\n```" language string)
         string))
      (t (lsp--render-element (lsp-ui-doc--inline-formatted-string string)))))))

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
   ((stringp contents) (lsp-ui-doc--extract-marked-string contents)) ;; MarkedString
   ((sequencep contents) ;; MarkedString[]
    (mapconcat 'lsp-ui-doc--extract-marked-string
               (lsp-ui-doc--filter-marked-string (seq-filter #'identity contents))
               "\n\n"
               ;; (propertize "\n\n" 'face '(:height 0.4))
               ))
   ;; when we get markdown contents, render using emacs gfm-view-mode / markdown-mode
   ((string= (gethash "kind" contents) "markdown") ;; Markdown MarkupContent
    (lsp-ui-doc--extract-marked-string (gethash "value" contents) "markdown"))
   ((gethash "kind" contents) (gethash "value" contents)) ;; Plaintext MarkupContent
   ((gethash "language" contents) ;; MarkedString
    (lsp-ui-doc--extract-marked-string (gethash "value" contents)
                                       (gethash "language" contents)))))

(defun lsp-ui-doc--webkit-run-xwidget ()
  "Launch embedded WebKit instance."
  (lsp-ui-doc--with-buffer
   (let ((inhibit-read-only t))
     (insert " ")
     (goto-char 1)
     (let ((id (make-xwidget
                'webkit
                nil
                1
                1
                nil
                (buffer-name))))
       (set-xwidget-query-on-exit-flag id nil)
       (put-text-property (point) (+ 1 (point))
                          'display (list 'xwidget ':xwidget id))
       (xwidget-webkit-mode)
       (xwidget-webkit-goto-uri (xwidget-at 1)
                                lsp-ui-doc-webkit-client-path)
       (lsp-ui-doc--webkit-set-background)
       (lsp-ui-doc--webkit-set-foreground)))))

(defun lsp-ui-doc--webkit-set-background ()
  "Set background color of the WebKit widget."
  (lsp-ui-doc--webkit-execute-script
   (format "document.body.style.background = '%s';"
           "#fdfdfd"
           ;; (face-attribute 'lsp-ui-doc-background :background)
           )))

(defun lsp-ui-doc--webkit-set-foreground ()
  "Set foreground color of the WebKit widget."
  (lsp-ui-doc--webkit-execute-script
   (format "document.body.style.color = '%s';"
           (face-attribute 'default :foreground))))

(defun lsp-ui-doc--webkit-get-xwidget ()
  "Return Xwidget instance."
  (lsp-ui-doc--with-buffer
   (xwidget-at 1)))

(defun lsp-ui-doc--webkit-execute-script (script &optional fn)
  "Execute SCRIPT in embedded Xwidget and run optional callback FN."
  (when-let* ((xw (lsp-ui-doc--webkit-get-xwidget)))
    (xwidget-webkit-execute-script xw script fn)))

(defun lsp-ui-doc--webkit-execute-script-rv (script)
  "Execute SCRIPT in embedded Xwidget synchronously."
  (when-let* ((xw (lsp-ui-doc--webkit-get-xwidget)))
    (xwidget-webkit-execute-script-rv xw script)))

(defun lsp-ui-doc--hide-frame ()
  "Hide the frame."
  (setq lsp-ui-doc--bounds nil)
  (when (overlayp lsp-ui-doc--inline-ov)
    (delete-overlay lsp-ui-doc--inline-ov))
  (when (lsp-ui-doc--get-frame)
    (unless lsp-ui-doc-use-webkit
      (lsp-ui-doc--with-buffer
       (erase-buffer)))
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
  (or
   (nth 2 (or (window-line-height line)
              (and (redisplay t)
                   (window-line-height line))))
   0))

(defun lsp-ui-doc--sideline-pos-y ()
  (-> (when (bound-and-true-p lsp-ui-sideline--occupied-lines)
        (-min lsp-ui-sideline--occupied-lines))
      (line-number-at-pos)
      (lsp-ui-doc--line-height)))

(defun lsp-ui-doc--webkit-resize-callback (size)
  (let ((offset-width (round (aref size 0)))
        (offset-height (round (aref size 1))))
    (xwidget-resize (lsp-ui-doc--webkit-get-xwidget) offset-width offset-height))
  (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))

(defun lsp-ui-doc--resize-buffer ()
  "If the buffer's width is larger than the current frame, resize it."
  (if lsp-ui-doc-use-webkit
      (lsp-ui-doc--webkit-execute-script
       "[document.querySelector('#lsp-ui-webkit').offsetWidth, document.querySelector('#lsp-ui-webkit').offsetHeight];"
       'lsp-ui-doc--webkit-resize-callback)

    (let* ((frame-width (frame-width))
           (fill-column (min lsp-ui-doc-max-width (- frame-width 5))))
      (when (> (lsp-ui-doc--buffer-width) (min lsp-ui-doc-max-width frame-width))
        (lsp-ui-doc--with-buffer
         (fill-region (point-min) (point-max)))))))

(defun lsp-ui-doc--mv-at-point (frame width height start-x start-y)
  "Move FRAME to be where the point is.
WIDTH is the child frame width.
HEIGHT is the child frame height.
START-X is the position x of the current window.
START-Y is the position y of the current window.
The algorithm prefers to position FRAME just above the
symbol at point, to not obstruct the view of the code that follows.
If there's no space above in the current window, it places
FRAME just below the symbol at point."
  (-let* (((x . y) (--> (bounds-of-thing-at-point 'symbol)
                        (posn-x-y (posn-at-point (car it)))))
          (frame-relative-symbol-x (+ start-x x))
          (frame-relative-symbol-y (+ start-y y))
          (char-height (frame-char-height))
          ;; Make sure the frame is positioned horizontally such that
          ;; it does not go beyond the frame boundaries.
          (frame-x (or (and (<= (frame-outer-width) (+ frame-relative-symbol-x width))
                            (- x (- (+ frame-relative-symbol-x width)
                                    (frame-outer-width))))
                       x))
          (frame-y (or (and (<= height frame-relative-symbol-y)
                            (- y height))
                       (+ y char-height))))
    (set-frame-position frame (+ start-x frame-x) (+ start-y frame-y))))

(defun lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (-let* (((left top right _bottom) (window-edges nil nil nil t))
          (window (frame-root-window frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
          (width (+ width (* (frame-char-width frame) 1))) ;; margins
          (char-h (frame-char-height))
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (frame-right (pcase lsp-ui-doc-alignment
                         ('frame (frame-pixel-width))
                         ('window right)))
          (frame-resize-pixelwise t))
    (set-frame-size frame width height t)
    (if (eq lsp-ui-doc-position 'at-point)
        (lsp-ui-doc--mv-at-point frame width height left top)
      (set-frame-position frame
                          (max (- frame-right width 10 (frame-char-width)) 10)
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
   (if lsp-ui-doc-use-webkit
       (progn
         (lsp-ui-doc--webkit-execute-script
          (format
           "renderMarkdown('%s', '%s');"
           symbol
           (url-hexify-string string))
          'lsp-ui-doc--webkit-resize-callback))
     (erase-buffer)
     (let ((inline-p (lsp-ui-doc--inline-p)))
       (insert (concat (unless inline-p (propertize "\n" 'face '(:height 0.2)))
                       (s-trim string)
                       (unless inline-p (propertize "\n\n" 'face '(:height 0.3))))))
     (lsp-ui-doc--make-clickable-link))
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
  (when (and lsp-ui-doc-use-webkit (not (featurep 'xwidget-internal)))
    (setq lsp-ui-doc-use-webkit nil))
  (if (or (null string) (string-empty-p string))
      (lsp-ui-doc--hide-frame)
    (lsp-ui-doc--render-buffer string symbol)
    (if (lsp-ui-doc--inline-p)
        (lsp-ui-doc--inline)
      (unless (lsp-ui-doc--get-frame)
        (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
      (unless lsp-ui-doc-use-webkit
        (lsp-ui-doc--resize-buffer)
        (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))
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
                         `((name . "")
                           (default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (left-fringe . ,(frame-char-width))
                           (background-color . ,(face-background 'lsp-ui-doc-background nil t)))))
         (window (display-buffer-in-child-frame
                  buffer
                  `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (with-current-buffer buffer
      (lsp-ui-doc-frame-mode 1))
    (set-frame-parameter nil 'lsp-ui-doc-buffer buffer)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-face-background 'internal-border lsp-ui-doc-border frame)
    (set-face-background 'fringe nil frame)
    (run-hook-with-args 'lsp-ui-doc-frame-hook frame window)
    (when lsp-ui-doc-use-webkit
      (define-key (current-global-map) [xwidget-event]
        (lambda ()
          (interactive)

          (let ((xwidget-event-type (nth 1 last-input-event)))
            ;; (when (eq xwidget-event-type 'load-changed)
            ;;   (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))

            (when (eq xwidget-event-type 'javascript-callback)
              (let ((proc (nth 3 last-input-event))
                    (arg (nth 4 last-input-event)))
                (funcall proc arg))))))
      (lsp-ui-doc--webkit-run-xwidget))
    frame))

(defun lsp-ui-doc--make-request nil
  "Request the documentation to the LS."
  (when (and (not (eq this-command 'lsp-ui-doc-hide))
             (not (bound-and-true-p lsp-ui-peek-mode))
             (lsp--capability "hoverProvider"))
    (-if-let (bounds (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                         (and (looking-at "[[:graph:]]") (cons (point) (1+ (point))))))
        (unless (equal lsp-ui-doc--bounds bounds)
          (lsp-ui-doc--hide-frame)
          (and lsp-ui-doc--timer (cancel-timer lsp-ui-doc--timer))
          (setq lsp-ui-doc--timer
                (run-with-idle-timer
                 lsp-ui-doc-delay nil
                 (let ((buf (current-buffer)))
                   (lambda nil
                     (when (equal buf (current-buffer))
                       (lsp--send-request-async
                        (lsp--make-request "textDocument/hover" (lsp--text-document-position-params))
                        (lambda (hover)
                          (when (equal buf (current-buffer))
                            (lsp-ui-doc--callback hover bounds (current-buffer)))))))))))
      (lsp-ui-doc--hide-frame))))

(defun lsp-ui-doc--callback (hover bounds buffer)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made."
  (if (and hover
           (>= (point) (car bounds)) (<= (point) (cdr bounds))
           (eq buffer (current-buffer)))
      (progn
        (setq lsp-ui-doc--bounds bounds)
        (lsp-ui-doc--display
         (thing-at-point 'symbol t)
         (-some->> (gethash "contents" hover)
           lsp-ui-doc--extract
           (replace-regexp-in-string "\r" ""))))
    (lsp-ui-doc--hide-frame)))

(defun lsp-ui-doc--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (lsp-ui-doc--get-frame))
    (delete-frame frame)
    (lsp-ui-doc--set-frame nil)))

(defun lsp-ui-doc--visible-p ()
  "Return whether the LSP UI doc is visible"
  (or (overlayp lsp-ui-doc--inline-ov)
      (and (lsp-ui-doc--get-frame)
           (frame-visible-p (lsp-ui-doc--get-frame)))))

(defun lsp-ui--hide-doc-frame-on-window-change (fun window &optional no-record)
  "Delete the child frame if currently selected window changes.
Does nothing if the newly-selected window is the same window as
before, or if the new window is the minibuffer."
  (let ((initial-window (selected-window)))
    (prog1 (funcall fun window no-record)
      (unless no-record
        (when (lsp-ui-doc--visible-p)
          (let* ((current-window (selected-window))
                 (doc-buffer (get-buffer (lsp-ui-doc--make-buffer-name))))
            (unless (or (window-minibuffer-p current-window)
                        (equal current-window initial-window)
                        (and doc-buffer
                             (equal (window-buffer initial-window) doc-buffer)))
              (lsp-ui-doc--hide-frame))))))))

(advice-add #'select-window :around #'lsp-ui--hide-doc-frame-on-window-change)

(advice-add 'load-theme :before (lambda (&rest _) (lsp-ui-doc--delete-frame)))
(add-hook 'window-configuration-change-hook #'lsp-ui-doc--hide-frame)

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
    (add-hook 'post-command-hook 'lsp-ui-doc--make-request nil t)
    (add-hook 'delete-frame-functions 'lsp-ui-doc--on-delete nil t))
   (t
    (lsp-ui-doc-hide)
    (remove-hook 'post-command-hook 'lsp-ui-doc--make-request t)
    (remove-hook 'delete-frame-functions 'lsp-ui-doc--on-delete t))))

(defun lsp-ui-doc-enable (enable)
  "Enable/disable ‘lsp-ui-doc-mode’.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-ui-doc-mode (if enable 1 -1)))

(defun lsp-ui-doc-show ()
  "Trigger display hover information popup."
  (interactive)
  (lsp-ui-doc--callback (lsp-request "textDocument/hover" (lsp--text-document-position-params))
                        (or (bounds-of-thing-at-point 'symbol) (cons (point) (1+ (point))))
                        (current-buffer)))

(defun lsp-ui-doc-hide ()
  "Hide hover information popup."
  (interactive)
  (lsp-ui-doc--hide-frame))

(defvar-local lsp-ui-doc--unfocus-frame-timer nil)
(defun lsp-ui-doc--glance-hide-frame ()
  "Hook to hide hover information popup for `lsp-ui-doc-glance'."
  (when (or (overlayp lsp-ui-doc--inline-ov)
            (lsp-ui-doc--frame-visible-p))
    (lsp-ui-doc--hide-frame)
    (remove-hook 'post-command-hook 'lsp-ui-doc--glance-hide-frame)
    ;; make sure child frame is unfocused
    (setq lsp-ui-doc--unfocus-frame-timer
          (run-at-time 1 nil #'lsp-ui-doc-unfocus-frame))))

(defun lsp-ui-doc-glance ()
  "Trigger display hover information popup and hide it on next typing."
  (interactive)
  (lsp-ui-doc--make-request)
  (when lsp-ui-doc--unfocus-frame-timer
    (cancel-timer lsp-ui-doc--unfocus-frame-timer))
  (add-hook 'post-command-hook 'lsp-ui-doc--glance-hide-frame))

(define-minor-mode lsp-ui-doc-frame-mode
  "Marker mode to add additional key bind for lsp-ui-doc-frame."
  :init-value nil
  :lighter ""
  :group lsp-ui-doc
  :keymap `(([?q] . lsp-ui-doc-unfocus-frame)))

(defun lsp-ui-doc-focus-frame ()
  "Focus into lsp-ui-doc-frame."
  (interactive)
  (when (lsp-ui-doc--frame-visible-p)
    (lsp-ui-doc--with-buffer
     (setq cursor-type t))
    (select-frame-set-input-focus (lsp-ui-doc--get-frame))))

(defun lsp-ui-doc-unfocus-frame ()
  "Unfocus from lsp-ui-doc-frame."
  (interactive)
  (when-let ((frame (frame-parent (lsp-ui-doc--get-frame))))
    (select-frame-set-input-focus frame)))

(provide 'lsp-ui-doc)
;;; lsp-ui-doc.el ends here
