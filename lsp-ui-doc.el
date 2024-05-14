;;; lsp-ui-doc.el --- Lsp-Ui-Doc  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languagues, tools
;; Version: 6.2

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

(require 'lsp-ui-util)

(require 'lsp-protocol)
(require 'lsp-mode)
(require 'dash)
(require 'goto-addr)
(require 'markdown-mode)

(require 'cl-lib)
(require 'face-remap)
(require 'subr-x)

(when (featurep 'xwidget-internal)
  (require 'xwidget))

(declare-function make-xwidget "ext:xwidget" (type title width height arguments &optional buffer))
(declare-function set-xwidget-query-on-exit-flag "ext:xwidget")
(declare-function xwidget-webkit-mode "ext:xwidget")
(declare-function xwidget-webkit-goto-uri "ext:xwidget" (xwidget uri))
(declare-function xwidget-at "ext:xwidget" (pos))
(declare-function xwidget-webkit-execute-script "ext:xwidget" (xwidget script &optional callback))
(declare-function xwidget-webkit-execute-script-rv "ext:xwidget" (xwidget script &optional default))
(declare-function xwidget-resize "ext:xwidget" (xwidget new-width new-height))

(defgroup lsp-ui-doc nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-doc) Top")
  :link '(info-link "(lsp-ui-doc) Customizing"))

(defcustom lsp-ui-doc-enable t
  "Whether or not to enable lsp-ui-doc.
Displays documentation of the symbol at point on hover.  This only
takes effect when a buffer is started."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-doc-show-with-mouse t
  "Move the mouse pointer over a symbol to show its documentation."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-show-with-cursor nil
  "Move the cursor over a symbol to show its documentation."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-header nil
  "Whether or not to enable the header which display the symbol string."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-include-signature nil
  "Whether or not to include the object signature/type in the frame."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-position 'top
  "Where to display the doc when moving the point cursor.
This affects the position of the documentation when
`lsp-ui-doc-show-with-cursor' is non-nil."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "At point" at-point))
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-side 'right
  "Which side to display the doc."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-alignment 'frame
  "How to align the doc.
This only takes effect when `lsp-ui-doc-position' is `top or `bottom."
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

(defcustom lsp-ui-doc-webkit-max-width-px 600
  "Maximum width in pixels for the webkit frame."
  :type 'integer
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-max-height 13
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

(defcustom lsp-ui-doc-enhanced-markdown t
  "Lsp-ui-doc will attempt to better format the markdown documentation."
  :type 'boolean
  :group 'lsp-ui-doc)

(defcustom lsp-ui-doc-text-scale-level 0
  "Text scale amount for doc buffer."
  :type 'integer
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

(defface lsp-ui-doc-highlight-hover
  '((t :inherit region))
  "Face used to highlight the hover symbol/region when using mouse."
  :group 'lsp-ui-doc)

(defface lsp-ui-doc-url
  '((t :inherit link))
  "Face used on links."
  :group 'lsp-ui-doc)

(defvar lsp-ui-doc-frame-parameters
  '((left                     . -1)
    (no-focus-on-map          . t)
    (min-width                . 0)
    (width                    . 0)
    (min-height               . 0)
    (height                   . 0)
    (internal-border-width    . 1)
    (vertical-scroll-bars     . nil)
    (horizontal-scroll-bars   . nil)
    (right-fringe             . 0)
    (menu-bar-lines           . 0)
    (tool-bar-lines           . 0)
    (tab-bar-lines            . 0)
    (tab-bar-lines-keep-state . 0)
    (line-spacing             . 0)
    (unsplittable             . t)
    (undecorated              . t)
    (top                      . -1)
    (visibility               . nil)
    (mouse-wheel-frame        . nil)
    (no-other-frame           . t)
    (inhibit-double-buffering . t)
    (drag-internal-border     . t)
    (no-special-glyphs        . t)
    (desktop-dont-save        . t))
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

(defvar-local lsp-ui-doc--highlight-ov nil
  "Overlay used to highlight the hover symbol.")

(defvar-local lsp-ui-doc--bounds nil)
(defvar-local lsp-ui-doc--timer nil)
(defvar-local lsp-ui-doc--from-mouse nil
  "Non nil when the doc was triggered by a mouse event.")
(defvar-local lsp-ui-doc--from-mouse-current nil
  "Non nil when the current call is triggered by a mouse event.")
(defvar-local lsp-ui-doc--hide-on-next-command nil
  "Non-nil when the current document should ask to hide after next command.")


(defconst lsp-ui-doc--buffer-prefix " *lsp-ui-doc-"
  "LSP UI doc prefix.")

(defmacro lsp-ui-doc--with-buffer (&rest body)
  "Execute BODY in the lsp-ui-doc buffer."
  (declare (indent 0) (debug t))
  `(let ((parent-vars (list :buffer (current-buffer)
                            :window (get-buffer-window)))
         (buffer-list-update-hook nil))
     (with-current-buffer (get-buffer-create (lsp-ui-doc--make-buffer-name))
       (setq lsp-ui-doc--parent-vars parent-vars)
       (setq left-margin-width 0)
       (setq right-margin-width 0)
       (prog1 (let ((buffer-read-only nil)
                    (inhibit-modification-hooks t)
                    (inhibit-redisplay t))
                ,@body)
         (setq buffer-read-only t)
         (let ((text-scale-mode-step 1.1))
           (text-scale-set lsp-ui-doc-text-scale-level))))))

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
;; Markdown 2.3.
(defvar markdown-fontify-code-block-default-mode)

(defsubst lsp-ui-doc--inline-visible-p ()
  "Return inline documentation visibility."
  (and (overlayp lsp-ui-doc--inline-ov) (overlay-buffer lsp-ui-doc--inline-ov)))

(defun lsp-ui-doc--inline-wrapped-line (string)
  "Wraps a line of text (STRING) for inline display."
  (cond ((string-empty-p string) "")
        (t string)))

(defun lsp-ui-doc--inline-formatted-string (string)
  "Formats STRING for inline rendering."
  (mapconcat (lambda (line)
               (lsp-ui-doc--inline-wrapped-line (string-trim-right line)))
             (split-string string "[\n\v\f\r]")
             "\n"))

(defun lsp-ui-doc--extract-marked-string (marked-string &optional language)
  "Render the MARKED-STRING with LANGUAGE."
  (string-trim-right
   (let* ((string (if (stringp marked-string)
                      marked-string
                    (lsp:markup-content-value marked-string)))
          (with-lang (lsp-marked-string? marked-string))
          (language (or (and with-lang
                             (or (lsp:marked-string-language marked-string)
                                 (lsp:markup-content-kind marked-string)))
                        language))
          (markdown-hr-display-char nil))
     (cond
      (lsp-ui-doc-use-webkit
       (if (and language
                (not (string= "text" language))
                (not (string= lsp/markup-kind-markdown language)))
           (format "```%s\n%s\n```" language string)
         string))
      ;; For other programming languages
      (language (lsp--render-string (lsp-ui-doc--inline-formatted-string string) language))
      ;; For default element content
      (t (lsp--render-element (lsp-ui-doc--inline-formatted-string string)))))))

(defun lsp-ui-doc--filter-marked-string (list-marked-string)
  "Filter the LIST-MARKED-STRING."
  (let ((groups (--separate (and (lsp-marked-string? it)
                                 (lsp-get-renderer (lsp:marked-string-language it)))
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
   ((vectorp contents) ;; MarkedString[]
    (mapconcat 'lsp-ui-doc--extract-marked-string
               (lsp-ui-doc--filter-marked-string (seq-filter #'identity contents))
               "\n\n"
               ;;(propertize "\n\n" 'face '(:height 0.4))
               ))
   ;; when we get markdown contents, render using emacs gfm-view-mode / markdown-mode
   ((and (lsp-marked-string? contents)
         (lsp:marked-string-language contents))
    (lsp-ui-doc--extract-marked-string (lsp:marked-string-value contents)
                                       (lsp:marked-string-language contents)))
   ;; The specification for MarkedString also includes raw strings of
   ;; markdown, which is not reflected by `lsp-marked-string?'
   ((stringp contents)
    (lsp-ui-doc--extract-marked-string contents lsp/markup-kind-markdown))
   ((lsp-marked-string? contents) (lsp-ui-doc--extract-marked-string contents))
   ((and (lsp-markup-content? contents)
         (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
    (lsp-ui-doc--extract-marked-string (lsp:markup-content-value contents) lsp/markup-kind-markdown))
   ((and (lsp-markup-content? contents)
         (string= (lsp:markup-content-kind contents) lsp/markup-kind-plain-text))
    (lsp:markup-content-value contents))))

(defun lsp-ui-doc--webkit-run-xwidget ()
  "Launch embedded WebKit instance."
  (lsp-ui-doc--with-buffer
    (let ((inhibit-read-only t))
      (insert " ")
      (goto-char 1)
      (let ((id (make-xwidget 'webkit nil 1 1 nil (buffer-name))))
        (set-xwidget-query-on-exit-flag id nil)
        (put-text-property (point) (+ 1 (point))
                           'display (list 'xwidget ':xwidget id))
        (xwidget-webkit-mode)
        (xwidget-webkit-goto-uri (xwidget-at 1)
                                 lsp-ui-doc-webkit-client-path)
        (lsp-ui-doc--webkit-set-width)
        (lsp-ui-doc--webkit-set-background)
        (lsp-ui-doc--webkit-set-foreground)))))

(defun lsp-ui-doc--webkit-set-width ()
  "Set webkit document max-width CSS property."
  (lsp-ui-doc--webkit-execute-script
   (format "document.documentElement.style.setProperty('--webkit-max-width-px', %d + 'px');"
           lsp-ui-doc-webkit-max-width-px)))

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
  (-when-let* ((xw (lsp-ui-doc--webkit-get-xwidget)))
    (xwidget-webkit-execute-script xw script fn)))

(defun lsp-ui-doc--webkit-execute-script-rv (script)
  "Execute SCRIPT in embedded Xwidget synchronously."
  (-when-let* ((xw (lsp-ui-doc--webkit-get-xwidget)))
    (xwidget-webkit-execute-script-rv xw script)))

(defvar-local lsp-ui-doc--unfocus-frame-timer nil)

(defun lsp-ui-doc--hide-frame (&optional _win)
  "Hide any documentation frame or overlay."
  (setq lsp-ui-doc--bounds nil
        lsp-ui-doc--from-mouse nil)
  (lsp-ui-util-safe-delete-overlay lsp-ui-doc--inline-ov)
  (lsp-ui-util-safe-delete-overlay lsp-ui-doc--highlight-ov)
  (remove-hook 'post-command-hook 'lsp-ui-doc--hide-frame)
  (when-let ((frame (lsp-ui-doc--get-frame)))
    (when (frame-visible-p frame)
      (make-frame-invisible frame)))
  (setq lsp-ui-doc--unfocus-frame-timer
        (run-at-time 0 nil #'lsp-ui-doc-unfocus-frame)))

(defun lsp-ui-doc--buffer-width ()
  "Calculate the max width of the buffer."
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
  "Mark as unused function."
  (-> (when (bound-and-true-p lsp-ui-sideline--occupied-lines)
        (-min lsp-ui-sideline--occupied-lines))
      (line-number-at-pos)
      (lsp-ui-doc--line-height)))

(defun lsp-ui-doc--webkit-resize-callback (size)
  "Callback when resizing using webkit depends on the SIZE."
  (let ((offset-width (round (aref size 0)))
        (offset-height (round (aref size 1))))
    (xwidget-resize (lsp-ui-doc--webkit-get-xwidget) offset-width offset-height))
  (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))

(defun lsp-ui-doc--scale-column-width (width)
  "Return WIDTH adjusted relative to the text scale."
  (floor (/ width (expt 1.1 lsp-ui-doc-text-scale-level))))

(defun lsp-ui-doc--resize-buffer ()
  "If the buffer's width is larger than the current frame, resize it."
  (if lsp-ui-doc-use-webkit
      (lsp-ui-doc--webkit-execute-script
       "[document.querySelector('#lsp-ui-webkit').offsetWidth, document.querySelector('#lsp-ui-webkit').offsetHeight];"
       'lsp-ui-doc--webkit-resize-callback)

    (let* ((frame-width (frame-width))
           (fill-column (lsp-ui-doc--scale-column-width (min lsp-ui-doc-max-width (- frame-width 5)))))
      (when (> (lsp-ui-doc--buffer-width) (min lsp-ui-doc-max-width frame-width))
        (lsp-ui-doc--with-buffer
          (fill-region (point-min) (point-max)))))))

(defun lsp-ui-doc--mv-at-point (frame width height start-x start-y)
  "Return position of FRAME to be where the point is.
WIDTH is the child frame width.
HEIGHT is the child frame height.
START-X is the position x of the current window.
START-Y is the position y of the current window.
The algorithm prefers to position FRAME just above the
symbol at point, to not obstruct the view of the code that follows.
If there's no space above in the current window, it places
FRAME just below the symbol at point."
  (-let* (((x . y) (--> (or lsp-ui-doc--bounds (bounds-of-thing-at-point 'symbol))
                        (or (posn-x-y (posn-at-point (car it)))
                            (if (< (car it) (window-start))
                                (cons 0 0)
                              (posn-x-y (posn-at-point (1- (window-end))))))))
          (char-width (frame-char-width))
          (char-height (frame-char-height))
          (sbw (with-selected-frame frame (or (window-scroll-bar-width) 0)))
          (sbh (with-selected-frame frame (or (window-scroll-bar-height) 0)))
          (frame-relative-symbol-x (+ start-x x (* char-width 2) sbw))
          (frame-relative-symbol-y (+ start-y y (- 0 sbh)))
          ;; Make sure the frame is positioned horizontally such that
          ;; it does not go beyond the frame boundaries.
          (frame-x (or (and (<= (frame-outer-width) (+ frame-relative-symbol-x width))
                            (- x (- (+ frame-relative-symbol-x width)
                                    (frame-outer-width))))
                       x))
          (frame-y (+ (or (and (<= height frame-relative-symbol-y)
                               (- y height sbh))
                          (+ y char-height))
                      (if (fboundp 'window-tab-line-height) (window-tab-line-height) 0))))
    (cons (+ start-x frame-x) (+ start-y frame-y))))

(defun lsp-ui-doc--size-and-pos-changed (frame left top width height)
  (-let (((prev-left . prev-top) (frame-position frame)))
    (not (and (= left prev-left)
              (= top prev-top)
              (= height (frame-text-height frame))
              (= width (frame-text-width frame))))))

(defun lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (-let* (((left top right _bottom) (window-edges nil t nil t))
          (window (frame-root-window frame))
          (char-h (frame-char-height frame))
          (char-w (frame-char-width frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
          (width (+ width (* char-w 1))) ;; margins
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (width (min width (* lsp-ui-doc-max-width char-w)))
          (frame-right (pcase lsp-ui-doc-alignment
                         ('frame (frame-pixel-width))
                         ('window right)))
          ((left . top) (if (eq lsp-ui-doc-position 'at-point)
                            (lsp-ui-doc--mv-at-point frame width height left top)
                          (cons (pcase lsp-ui-doc-side
                                  ('right (max (- frame-right width char-w) 10))
                                  ('left 10))
                                (pcase lsp-ui-doc-position
                                  ('top (+ top char-w))
                                  ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                                              height
                                              10))))))
          (frame-resize-pixelwise t)
          (move-frame-functions nil)
          (window-size-change-functions nil)
          (window-state-change-hook nil)
          (window-state-change-functions nil)
          (window-configuration-change-hook nil)
          (inhibit-redisplay t))
    ;; Dirty way to fix unused variable in emacs 26
    (and window-state-change-functions
         window-state-change-hook)
    ;; Make frame invisible before moving/resizing it to avoid flickering:
    ;; We set the position and size in 1 call, modify-frame-parameters, but
    ;; internally emacs makes 2 different calls, which can be visible
    ;; to the user
    (and (frame-visible-p frame)
         (lsp-ui-doc--size-and-pos-changed frame left top width height)
         (make-frame-invisible frame))
    (modify-frame-parameters
     frame
     `((width . (text-pixels . ,width))
       (height . (text-pixels . ,height))
       (user-size . t)
       (left . (+ ,left))
       (top . (+ ,top))
       (user-position . t)
       (lsp-ui-doc--window-origin . ,(selected-window))
       (lsp-ui-doc--buffer-origin . ,(current-buffer))
       (lsp-ui-doc--no-focus . t)
       (right-fringe . 0)
       (left-fringe . 0)))
    ;; Insert hr lines after width is computed
    (lsp-ui-doc--fix-hr-props)
    ;; Force window to use buffer's margin settings instead of the
    ;; parent window's settings.
    (let ((window (frame-root-window frame))
          (buffer (get-buffer (lsp-ui-doc--make-buffer-name))))
      (set-window-buffer window buffer))
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(defun lsp-ui-doc--visit-file (filename)
  "Visit FILENAME in the parent frame."
  (-some->> (find-file-noselect filename)
    (set-window-buffer (lsp-ui-doc--get-parent :window))))

(defun lsp-ui-doc--put-click (start end fn)
  "Add text properties on text to make it clickable.
The text delimiters bound from START to END.
FN is the function to call on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] fn)
    (put-text-property start end 'keymap map)
    (put-text-property start end 'mouse-face
                       (list :inherit 'lsp-ui-doc-url
                             :box (list :line-width -1
                                        :color (face-foreground 'lsp-ui-doc-url))))
    (add-face-text-property start end 'lsp-ui-doc-url)))

(defun lsp-ui-doc--open-markdown-link (&rest _)
  (interactive "P")
  (let ((buffer-list-update-hook nil))
    (-let [(buffer point) (if-let* ((valid (and (listp last-input-event)
                                                (eq (car last-input-event) 'mouse-2)))
                                    (event (cadr last-input-event))
                                    (win (posn-window event))
                                    (buffer (window-buffer win)))
                              `(,buffer ,(posn-point event))
                            `(,(current-buffer) ,(point)))]
      (with-current-buffer buffer
        ;; Markdown-mode puts the url in 'help-echo
        (-some--> (get-text-property point 'help-echo)
          (and (string-match-p goto-address-url-regexp it)
               (browse-url it)))))))

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

(defun lsp-ui-doc--buffer-pre-command (&rest _)
  (and (not (eq this-command 'mwheel-scroll))
       (frame-parameter nil 'lsp-ui-doc--no-focus)
       (select-frame (frame-parent) t)))

(defun lsp-ui-doc--fill-document ()
  "Better wrap the document so it fits the doc window."
  (let ((fill-column (lsp-ui-doc--scale-column-width (- lsp-ui-doc-max-width 5)))
        start        ; record start for `fill-region'
        first-line)  ; first line in paragraph
    (save-excursion
      (goto-char (point-min))
      (setq start (point)
            first-line (thing-at-point 'line))
      (while (re-search-forward "^[ \t]*\n" nil t)
        (setq first-line (thing-at-point 'line))
        (when (< fill-column (length first-line))
          (fill-region start (point)))
        (setq start (point)))
      ;; Fill the last paragraph
      (when (< fill-column (length first-line))
        (fill-region start (point-max))))))

(defun lsp-ui-doc--make-smaller-empty-lines ()
  "Make empty lines half normal lines."
  (progn  ; Customize line before header
    (goto-char 1)
    (insert (propertize "\n" 'face '(:height 0.3))))
  (progn  ; Customize line after header
    (forward-line 1)
    (insert (propertize " " 'face '(:height 0.1))))
  (while (not (eobp))
    (when (and (eolp) (not (bobp)))
      (save-excursion
        (delete-region (point) (progn (forward-visible-line 1) (point))))
      (when (or (and (not (get-text-property (point) 'markdown-heading))
                     (not (get-text-property (max (- (point) 2) 1) 'markdown-heading)))
                (get-text-property (point) 'markdown-hr))
        (insert (propertize " " 'face `(:height 0.2))
                (propertize "\n" 'face '(:height 0.4)))))
    (forward-line))
  (insert (propertize "\n\n" 'face '(:height 0.3))))

(defun lsp-ui-doc--fix-hr-props ()
  ;; We insert the right display prop after window-text-pixel-size
  (lsp-ui-doc--with-buffer
    (let (next)
      (while (setq next (next-single-property-change (or next 1) 'lsp-ui-doc--replace-hr))
        (when (get-text-property next 'lsp-ui-doc--replace-hr)
          (put-text-property next (1+ next) 'display
                             '(space :align-to (- right-fringe 1) :height (1)))
          (put-text-property (1+ next) (+ next 2) 'display
                             '(space :align-to right-fringe :height (1))))))))

(defun lsp-ui-doc--handle-hr-lines nil
  (let (bolp next before after)
    (goto-char 1)
    (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
      (when (get-text-property next 'markdown-hr)
        (goto-char next)
        (setq bolp (bolp)
              before (char-before))
        (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
        (setq after (char-after (1+ (point))))
        (insert
         (concat
          (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
          (propertize " "
                      ;; :align-to is added with lsp-ui-doc--fix-hr-props
                      'display '(space :height (1))
                      'lsp-ui-doc--replace-hr t
                      'face '(:background "dark grey"))
          ;; :align-to is added here too
          (propertize " " 'display '(space :height (1)))
          (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.2)))))))))

(defun lsp-ui-doc--render-buffer (string symbol)
  "Set the buffer with STRING and SYMBOL."
  (lsp-ui-doc--with-buffer
    (if lsp-ui-doc-use-webkit
        (progn
          (lsp-ui-doc--webkit-execute-script
           (format "renderMarkdown('%s', '%s');"
                   symbol
                   (url-hexify-string string))
           'lsp-ui-doc--webkit-resize-callback))
      (erase-buffer)
      (insert (s-trim string))
      (unless (or (lsp-ui-doc--inline-p) (not lsp-ui-doc-enhanced-markdown))
        (lsp-ui-doc--fill-document)
        (lsp-ui-doc--make-smaller-empty-lines)
        (lsp-ui-doc--handle-hr-lines))
      (add-text-properties 1 (point) '(line-height 1))
      (lsp-ui-doc--make-clickable-link)
      (add-text-properties 1 (point-max) '(pointer arrow)))
    (lsp-ui-doc-frame-mode 1)
    (setq wrap-prefix '(space :height (1) :width 1)
          line-prefix '(space :height (1) :width 1))
    (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
    (setq-local window-min-height 1)
    (setq-local show-trailing-whitespace nil)
    (setq-local window-configuration-change-hook nil)
    (add-hook 'pre-command-hook 'lsp-ui-doc--buffer-pre-command nil t)
    (when (boundp 'window-state-change-functions)
      (setq-local window-state-change-functions nil))
    (when (boundp 'window-state-change-hook)
      (setq-local window-state-change-hook nil))
    (setq-local window-size-change-functions nil)
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

(defun lsp-ui-doc--inline-window-width ()
  (- (min (window-text-width) (window-body-width))
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
    merged))

(defun lsp-ui-doc--inline-pos-at (start lines)
  "Calcul the position at START + forward n LINES."
  (save-excursion (goto-char start) (forward-line lines) (point)))

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
          (ov (if (overlayp lsp-ui-doc--inline-ov)
                  (progn
                    (move-overlay lsp-ui-doc--inline-ov start end)
                    lsp-ui-doc--inline-ov)
                (setq lsp-ui-doc--inline-ov (make-overlay start end)))))
    (overlay-put ov 'face 'default)
    (overlay-put ov 'display (lsp-ui-doc--inline-merge buffer-string))
    (overlay-put ov 'lsp-ui-doc-inline t)
    (overlay-put ov 'window (selected-window))))

(defun lsp-ui-doc--inline-p ()
  "Return non-nil when the documentation should be displayed without a child frame."
  (or (not lsp-ui-doc-use-childframe)
      (not (display-graphic-p))
      (not (fboundp 'display-buffer-in-child-frame))))

(defun lsp-ui-doc--highlight-hover ()
  (when lsp-ui-doc--from-mouse-current
    (-let* (((start . end) lsp-ui-doc--bounds)
            (ov (if (overlayp lsp-ui-doc--highlight-ov) lsp-ui-doc--highlight-ov
                  (setq lsp-ui-doc--highlight-ov (make-overlay start end)))))
      (move-overlay ov start end)
      (overlay-put ov 'face 'lsp-ui-doc-highlight-hover)
      (overlay-put ov 'window (selected-window)))))

(defun lsp-ui-doc--display (symbol string)
  "Display the documentation."
  (when (and lsp-ui-doc-use-webkit (not (featurep 'xwidget-internal)))
    (setq lsp-ui-doc-use-webkit nil))
  (if (or (null string) (string-empty-p string))
      (lsp-ui-doc--hide-frame)
    (lsp-ui-doc--highlight-hover)
    (lsp-ui-doc--render-buffer string symbol)
    (if (lsp-ui-doc--inline-p)
        (lsp-ui-doc--inline)
      (unless (lsp-ui-doc--get-frame)
        (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
      (unless lsp-ui-doc-use-webkit
        (lsp-ui-doc--resize-buffer)
        (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame))))
    (setq lsp-ui-doc--from-mouse lsp-ui-doc--from-mouse-current)))

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
                           (left-fringe . 0)
                           (right-fringe . 0)
                           (cursor-type . nil)
                           (lsp-ui-doc--no-focus . t)
                           (background-color . ,(face-background 'lsp-ui-doc-background nil t)))))
         (window (display-buffer-in-child-frame
                  buffer
                  `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (with-current-buffer buffer
      (lsp-ui-doc-frame-mode 1))
    (set-frame-parameter nil 'lsp-ui-doc-buffer buffer)
    (set-window-dedicated-p window t)
    ;;(redirect-frame-focus frame (frame-parent frame))
    (set-face-background 'internal-border lsp-ui-doc-border frame)
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border lsp-ui-doc-border frame))
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

(defconst lsp-ui-doc--ignore-commands
  '(lsp-ui-doc-hide
    lsp-ui-doc--handle-mouse-movement
    keyboard-quit
    ignore
    handle-switch-frame
    mwheel-scroll)
  "List of command to ignore requests.")

(defun lsp-ui-doc--make-request ()
  "Request the documentation to the LS."
  (and (not track-mouse) lsp-ui-doc-show-with-mouse (setq-local track-mouse t))
  (when (and lsp-ui-doc-show-with-cursor
             (not (memq this-command lsp-ui-doc--ignore-commands))
             (not (bound-and-true-p lsp-ui-peek-mode))
             (lsp-feature? "textDocument/hover"))
    (-if-let (bounds (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                         (and (looking-at "[[:graph:]]") (cons (point) (1+ (point))))))
        (unless (and (equal lsp-ui-doc--bounds bounds) (not lsp-ui-doc--hide-on-next-command))
          (lsp-ui-doc--hide-frame)
          (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer)
          (setq lsp-ui-doc--timer
                (run-with-idle-timer
                 lsp-ui-doc-delay nil
                 (let ((buf (current-buffer))
                       (hide lsp-ui-doc--hide-on-next-command))
                   (lambda nil
                     (when (equal buf (current-buffer))
                       (lsp-request-async
                        "textDocument/hover"
                        (lsp--text-document-position-params)
                        (lambda (hover)
                          (when (equal buf (current-buffer))
                            (lsp-ui-doc--callback hover bounds (current-buffer) hide)))
                        :mode 'tick
                        :cancel-token :lsp-ui-doc-hover)))))))
      (lsp-ui-doc--hide-frame))))

(defun lsp-ui-doc--extract-bounds (hover)
  (-when-let* ((hover hover)
               (data (lsp-get hover :range))
               (start (-some-> (lsp:range-start data) lsp--position-to-point))
               (end (-some-> (lsp:range-end data) lsp--position-to-point)))
    (cons start end)))

(lsp-defun lsp-ui-doc--callback ((hover &as &Hover? :contents) bounds buffer hide)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made.
When HIDE is non-nil, hide the doc on next command."
  (let ((bounds (or (lsp-ui-doc--extract-bounds hover) bounds)))
    (if (and hover
             (>= (point) (car bounds))
             (<= (point) (cdr bounds))
             (eq buffer (current-buffer)))
        (progn
          (lsp-ui-util-safe-kill-timer lsp-ui-doc--unfocus-frame-timer)
          (when hide
            (add-hook 'post-command-hook 'lsp-ui-doc--hide-frame))
          (setq lsp-ui-doc--bounds bounds)
          (lsp-ui-doc--display
           (thing-at-point 'symbol t)
           (-some->> contents
             lsp-ui-doc--extract
             (replace-regexp-in-string "\r" "")
             (replace-regexp-in-string " " " "))))
      (lsp-ui-doc--hide-frame))))

(defun lsp-ui-doc--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (lsp-ui-doc--get-frame))
    (delete-frame frame)
    (lsp-ui-doc--set-frame nil)))

(defun lsp-ui-doc--visible-p ()
  "Return whether the LSP UI doc is visible"
  (or (lsp-ui-doc--inline-visible-p)
      (lsp-ui-doc--frame-visible-p)))

(defun lsp-ui-doc-hide-frame-on-window-change (fun window &optional no-record)
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

(unless (boundp 'window-state-change-functions)
  (advice-add #'select-window :around #'lsp-ui-doc-hide-frame-on-window-change)
  (add-hook 'window-configuration-change-hook #'lsp-ui-doc--hide-frame))

(defvar-local lsp-ui-doc--timer-on-changes nil)

(defun lsp-ui-doc--on-state-changed (_frame &optional on-idle)
  (-when-let* ((frame (lsp-ui-doc--get-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame)
         (not (minibufferp (window-buffer)))
         (or (not (eq (selected-window) (frame-parameter frame 'lsp-ui-doc--window-origin)))
             (not (eq (window-buffer) (frame-parameter frame 'lsp-ui-doc--buffer-origin))))
         (if on-idle (lsp-ui-doc--hide-frame)
           (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer-on-changes)
           (setq lsp-ui-doc--timer-on-changes
                 (run-with-idle-timer 0 nil (lambda nil (lsp-ui-doc--on-state-changed frame t))))))))

(advice-add 'load-theme :before (lambda (&rest _) (lsp-ui-doc--delete-frame)))

(advice-add #'keyboard-quit :before #'lsp-ui-doc--hide-frame)

(defun lsp-ui-doc--on-delete (frame)
  "Function called when a FRAME is deleted."
  (-some--> (frame-parameter frame 'lsp-ui-doc-buffer)
    (get-buffer it)
    (and (buffer-live-p it) it)
    (kill-buffer it)))

(defun lsp-ui-doc--handle-scroll (win _new-start)
  "Handle scrolling to the document frame.

This function is apply to hook `window-scroll-functions'.

Argument WIN is current applying window."
  (let ((frame (lsp-ui-doc--get-frame)))
    (if (minibufferp (window-buffer))
        (lsp-ui-doc--hide-frame)
      (when (and frame
                 (eq lsp-ui-doc-position 'at-point)
                 (frame-visible-p frame)
                 (eq win (selected-window)))  ; This resolved #524
        (if (and lsp-ui-doc--bounds
                 (eq (window-buffer) (frame-parameter frame 'lsp-ui-doc--buffer-origin))
                 (>= (point) (car lsp-ui-doc--bounds))
                 (<= (point) (cdr lsp-ui-doc--bounds)))
            (lsp-ui-doc--move-frame frame)
          ;; The point might have changed if the window was scrolled
          ;; too far
          (lsp-ui-doc--hide-frame))))))

(defvar-local lsp-ui-doc--timer-mouse-movement nil)
(defvar-local lsp-ui-doc--last-event nil)

(defun lsp-ui-doc--mouse-display nil
  (when (and lsp-ui-doc--last-event
             (lsp-feature? "textDocument/hover"))
    (save-excursion
      (goto-char lsp-ui-doc--last-event)
      (-when-let* ((valid (not (eolp)))
                   (bounds (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                               (and (looking-at "[[:graph:]]") (cons (point) (1+ (point)))))))
        (unless (equal bounds lsp-ui-doc--bounds)
          (lsp-request-async
           "textDocument/hover"
           (lsp--text-document-position-params)
           (lambda (hover)
             (save-excursion
               (goto-char lsp-ui-doc--last-event)
               (let ((lsp-ui-doc-position 'at-point)
                     (lsp-ui-doc--from-mouse-current t))
                 (lsp-ui-doc--callback hover bounds (current-buffer) nil))))
           :mode 'tick
           :cancel-token :lsp-ui-doc-hover))))))

(defun lsp-ui-doc--tooltip-mouse-motion (event)
  "Default tooltip (EVENT) action."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (tooltip-start-delayed-tip)
    (setq tooltip-last-mouse-motion-event event)))

(defun lsp-ui-doc--handle-mouse-movement (event)
  "Show the documentation corresponding to the text under EVENT."
  (interactive "e")
  (lsp-ui-doc--tooltip-mouse-motion event)
  (when lsp-ui-doc-show-with-mouse
    (lsp-ui-util-safe-kill-timer lsp-ui-doc--timer-mouse-movement)
    (let* ((e (cadr event))
           (point (posn-point e))
           (same-win (eq (selected-window) (posn-window e))))
      (and lsp-ui-doc--from-mouse
           lsp-ui-doc--bounds
           point
           (or (< point (car lsp-ui-doc--bounds))
               (> point (cdr lsp-ui-doc--bounds))
               (not same-win)
               (equal (char-after point) ?\n))
           (lsp-ui-doc--hide-frame))
      (when same-win
        (setq lsp-ui-doc--last-event point
              lsp-ui-doc--timer-mouse-movement
              (run-with-idle-timer lsp-ui-doc-delay nil 'lsp-ui-doc--mouse-display))))))

(defun lsp-ui-doc--disable-mouse-on-prefix nil
  (and (bound-and-true-p lsp-ui-doc-mode)
       (bound-and-true-p lsp-ui-doc--mouse-tracked-by-us)
       track-mouse
       (> (length (this-single-command-keys)) 0)
       (setq-local track-mouse nil)))

(defvar lsp-ui-doc--timer-mouse-idle nil)

(defvar-local lsp-ui-doc--mouse-tracked-by-us nil
  "Nil if `track-mouse' was set by another package.
If nil, do not prevent mouse on prefix keys.")

(defvar lsp-ui-doc-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `lsp-ui-doc-mode'.")

(defun lsp-ui-doc--setup-mouse ()
  "Setup mouse."
  (cond
   (lsp-ui-doc-show-with-mouse
    (define-key lsp-ui-doc-mode-map (kbd "<mouse-movement>") #'lsp-ui-doc--handle-mouse-movement)
    (setq lsp-ui-doc--mouse-tracked-by-us (not track-mouse))
    (setq-local track-mouse t)
    (unless lsp-ui-doc--timer-mouse-idle
      ;; Set only 1 timer for all buffers
      (setq lsp-ui-doc--timer-mouse-idle
            (run-with-idle-timer 0 t 'lsp-ui-doc--disable-mouse-on-prefix))))
   (t
    (define-key lsp-ui-doc-mode-map (kbd "<mouse-movement>") nil))))

(defun lsp-ui-doc--prevent-focus-doc (e)
  (not (frame-parameter (cadr e) 'lsp-ui-doc--no-focus)))

(define-minor-mode lsp-ui-doc-mode
  "Minor mode for showing hover information in child frame."
  :init-value nil
  :keymap lsp-ui-doc-mode-map
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
    (when (boundp 'window-state-change-functions)
      (add-hook 'window-state-change-functions 'lsp-ui-doc--on-state-changed))
    (lsp-ui-doc--setup-mouse)
    (advice-add 'handle-switch-frame :before-while 'lsp-ui-doc--prevent-focus-doc)
    (add-hook 'post-command-hook 'lsp-ui-doc--make-request nil t)
    (add-hook 'window-scroll-functions 'lsp-ui-doc--handle-scroll nil t)
    (add-hook 'delete-frame-functions 'lsp-ui-doc--on-delete nil t))
   (t
    (lsp-ui-doc-hide)
    (when (boundp 'window-state-change-functions)
      (remove-hook 'window-state-change-functions 'lsp-ui-doc--on-state-changed))
    (remove-hook 'window-scroll-functions 'lsp-ui-doc--handle-scroll t)
    (remove-hook 'post-command-hook 'lsp-ui-doc--make-request t)
    (remove-hook 'delete-frame-functions 'lsp-ui-doc--on-delete t))))

(defun lsp-ui-doc-enable (enable)
  "Enable/disable ‘lsp-ui-doc-mode’.
It is supposed to be called from `lsp-ui--toggle'"
  (lsp-ui-doc-mode (if enable 1 -1)))

(defun lsp-ui-doc-show ()
  "Trigger display hover information popup."
  (interactive)
  (let ((lsp-ui-doc-show-with-cursor t)
        (lsp-ui-doc-delay 0))
    (lsp-ui-doc--make-request)))

(defun lsp-ui-doc-hide ()
  "Hide hover information popup."
  (interactive)
  (lsp-ui-doc-unfocus-frame) ;; In case focus is in doc frame
  (lsp-ui-doc--hide-frame))

(defun lsp-ui-doc-toggle ()
  "Toggle hover information popup."
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

(defun lsp-ui-doc-glance ()
  "Trigger display hover information popup and hide it on next typing."
  (interactive)
  (let ((lsp-ui-doc--hide-on-next-command t))
    (lsp-ui-doc-show)))

(define-minor-mode lsp-ui-doc-frame-mode
  "Marker mode to add additional key bind for lsp-ui-doc-frame."
  :init-value nil
  :lighter ""
  :group lsp-ui-doc
  :keymap `(([?q] . lsp-ui-doc-unfocus-frame)
            ([remap markdown-follow-thing-at-point] . lsp-ui-doc--open-markdown-link)
            ([remap mouse-drag-region] . ignore)))

(defun lsp-ui-doc-focus-frame ()
  "Focus into lsp-ui-doc-frame."
  (interactive)
  (when-let* ((frame (lsp-ui-doc--get-frame))
              (visible (lsp-ui-doc--frame-visible-p)))
    (remove-hook 'post-command-hook 'lsp-ui-doc--hide-frame)
    (set-frame-parameter frame 'lsp-ui-doc--no-focus nil)
    (set-frame-parameter frame 'cursor-type t)
    (lsp-ui-doc--with-buffer
      (setq cursor-type 'box))
    (select-frame-set-input-focus frame)))

(defun lsp-ui-doc-unfocus-frame ()
  "Unfocus from lsp-ui-doc-frame."
  (interactive)
  (-some-> (frame-parent) select-frame-set-input-focus)
  (when-let* ((frame (lsp-ui-doc--get-frame)))
    (set-frame-parameter frame 'lsp-ui-doc--no-focus t)
    (set-frame-parameter frame 'cursor-type nil)
    (lsp-ui-doc--with-buffer
      (setq cursor-type nil))
    (when lsp-ui-doc--from-mouse
      (make-frame-invisible frame))))

(provide 'lsp-ui-doc)
;;; lsp-ui-doc.el ends here
