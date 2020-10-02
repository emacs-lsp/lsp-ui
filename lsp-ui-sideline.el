;;; lsp-ui-sideline.el --- Lsp-Ui-Sideline  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languages, tools
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
;; Utility to show information for the current line

;;; Code:

(require 'lsp-protocol)
(require 'lsp-mode)
(require 'flycheck nil 'noerror)
(require 'dash)
(require 'seq)
(require 'subr-x)
(require 'face-remap)

(defgroup lsp-ui-sideline nil
  "Display information for the current line."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-sideline) Top")
  :link '(info-link "(lsp-ui-sideline) Customizing"))

(defcustom lsp-ui-sideline-enable t
  "Whether or not to enable ‘lsp-ui-sideline’."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-sideline-ignore-duplicate nil
  "Ignore duplicates when there is a same symbol with the same contents."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-symbol t
  "When t, show the symbol name on the right of the information."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-hover nil
  "Whether to show hover messages in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-diagnostics t
  "Whether to show diagnostics messages in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-code-actions t
  "Whether to show code actions in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-update-mode 'point
  "Define the mode for updating sideline actions.

When set to `line' the actions will be updated when user
changes current line otherwise the actions will be updated
when user changes current point."
  :type '(choice (const line)
                 (const point))
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-delay 0.2
  "Number of seconds to wait before showing sideline."
  :type 'number
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-diagnostic-max-lines 1
  "Maximum number of lines to show of diagnostics in sideline."
  :type 'integer
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-diagnostic-max-line-length 100
  "Maximum line length of diagnostics in sideline."
  :type 'integer
  :group 'lsp-ui-sideline)

(defconst lsp-ui-sideline-actions-icon-default
  (and (bound-and-true-p lsp-ui-resources-dir)
       (image-type-available-p 'png)
       (expand-file-name "lightbulb.png" lsp-ui-resources-dir)))

(defcustom lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default
  "Image file for actions.  It must be a png file."
  :type '(choice file (const :tag "Disable" nil))
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-wait-for-all-symbols t
  "Wait for all symbols before displaying info in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-actions-kind-regex "quickfix.*\\|refactor.*"
  "Regex for the code actions kinds to show in the sideline."
  :type 'string
  :group 'lsp-ui-sideline)

(defvar lsp-ui-sideline-code-actions-prefix ""
  "Prefix to insert before the code action title.
This can be used to insert, for example, an unicode character: 💡")

(defvar-local lsp-ui-sideline--ovs nil
  "Overlays used by `lsp-ui-sideline'.")

(defvar-local lsp-ui-sideline--occupied-lines nil
  "List of lines occupied by an overlay of `lsp-ui-sideline'.")

(defvar-local lsp-ui-sideline--tag nil
  "Tag marking where the last operation was based.
It is used to know when the cursor has changed its line or point.")

(defvar-local lsp-ui-sideline--last-width nil
  "Value of window's width on the last operation.
It is used to know when the window has changed of width.")

(defvar-local lsp-ui-sideline--timer nil)

(defvar-local lsp-ui-sideline--code-actions nil
  "Holds the latest code actions.")

(defface lsp-ui-sideline-symbol
  '((t :foreground "grey"
       :box (:line-width -1 :color "grey")
       :height 0.99))
  "Face used to highlight symbols."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-current-symbol
  '((default
      :foreground "white"
      :weight ultra-bold
      :box (:line-width -1 :color "white")
      :height 0.99)
    (((background light))
     :foreground "dim gray"
     :box (:line-width -1 :color "dim gray")))
  "Face used to highlight the symbol on point."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-code-action
  '((default :foreground "yellow")
    (((background light)) :foreground "DarkOrange"))
  "Face used to highlight code action text."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-symbol-info
  '((t :slant italic :height 0.99))
  "Face used to highlight the symbols informations (LSP hover)."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-global
  '((t))
  "Face which apply to all overlays.
This face have a low priority over the others."
  :group 'lsp-ui-sideline)

(defun lsp-ui-sideline--first-line-p (pos)
  "Return non-nil if POS is on the first line."
  (save-excursion
    (goto-char 1)
    (forward-line)
    (> (point) pos)))

(defun lsp-ui-sideline--calc-space (win-width str-len index)
  "Calcul whether there is enough space on line.
If there is enough space, it returns the point of the last
character on the line.

WIN-WIDTH is the window width.
STR-LEN is the string size.
INDEX is the line number (relative to the current line)."
  (let ((eol (line-end-position index)))
    (unless (member eol lsp-ui-sideline--occupied-lines)
      (save-excursion
        (goto-char eol)
        (when (>= (- win-width (current-column)) str-len)
          eol)))))

(defun lsp-ui-sideline--find-line (str-len bol eol &optional up offset)
  "Find a line where the string can be inserted.
It loops on the nexts lines to find enough space.
Returns the point of the last character on the line.

STR-LEN is the string size.
BOL & EOL are beginning and ending of the user point line.
if UP is non-nil, it loops on the previous lines.
if OFFSET is non-nil, it starts search OFFSET lines from user point line."
  (let ((win-width (lsp-ui-sideline--window-width))
        (index (if (null offset) 1 offset))
        pos)
    (while (and (null pos) (<= (abs index) 30))
      (setq index (if up (1- index) (1+ index)))
      (setq pos (lsp-ui-sideline--calc-space win-width str-len index)))
    (if (and up (or (null pos) (and (lsp-ui-sideline--first-line-p pos)
                                    ;; line-end-position returns a wrong value when its
                                    ;; argument lead to a line < 0, so we need to use this trick
                                    (-any-p 'lsp-ui-sideline--first-line-p lsp-ui-sideline--occupied-lines))))
        (lsp-ui-sideline--find-line str-len bol eol nil offset)
      (and pos (or (> pos eol) (< pos bol))
           (push pos lsp-ui-sideline--occupied-lines)
           (list pos (1- index))))))

(defun lsp-ui-sideline--delete-ov ()
  "Delete overlays."
  (seq-do 'delete-overlay lsp-ui-sideline--ovs)
  (setq lsp-ui-sideline--tag nil
        lsp-ui-sideline--occupied-lines nil
        lsp-ui-sideline--ovs nil))

(defun lsp-ui-sideline--extract-info (contents)
  "Extract the line to print from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We prioritize string with a language (which is probably a type or a
function signature)."
  (when contents
    (cond
     ((lsp-marked-string? contents) contents)
     ((vectorp contents)
      (seq-find (lambda (it) (and (lsp-marked-string? it)
                                  (lsp-get-renderer (lsp:marked-string-language it))))
                contents))
     ((lsp-markup-content? contents) contents))))

(defun lsp-ui-sideline--format-info (marked-string win-width)
  "Format MARKED-STRING.
If the string has a language, we fontify it with the function provided
by `lsp-mode'.
MARKED-STRING is the string returned by `lsp-ui-sideline--extract-info'."
  (when (and marked-string (or (lsp-marked-string? marked-string) (lsp-markup-content? marked-string)))
    (setq marked-string (lsp--render-element marked-string))
    (add-face-text-property 0 (length marked-string) 'lsp-ui-sideline-symbol-info nil marked-string)
    (add-face-text-property 0 (length marked-string) 'default t marked-string)
    (->> (if (> (length marked-string) win-width)
             (car (split-string marked-string "[\r\n]+"))
           marked-string)
         (replace-regexp-in-string "[\n\r\t ]+" " "))))

(defun lsp-ui-sideline--align (&rest lengths)
  (+ (apply '+ lengths)
     (if (display-graphic-p) 1 2)))

(defun lsp-ui-sideline--compute-height nil
  "Return a fixed size for text in sideline."
  (if (null text-scale-mode-remapping)
      '(height 1)
    ;; Readjust height when text-scale-mode is used
    (list 'height
          (/ 1 (or (plist-get (cdr text-scale-mode-remapping) :height)
                   1)))))

(defun lsp-ui-sideline--make-display-string (info symbol current)
  "Make final string to display in buffer.
INFO is the information to display.
SYMBOL is the symbol associated with the info.
CURRENT is non-nil when the point is on the symbol."
  (let* ((face (if current 'lsp-ui-sideline-current-symbol 'lsp-ui-sideline-symbol))
         (str (if lsp-ui-sideline-show-symbol
                  (concat info " " (propertize (concat " " symbol " ") 'face face))
                info))
         (len (length str))
         (margin (lsp-ui-sideline--margin-width)))
    (add-face-text-property 0 len 'lsp-ui-sideline-global nil str)
    (concat
     (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
     (propertize str 'display (lsp-ui-sideline--compute-height)))))

(defun lsp-ui-sideline--check-duplicate (symbol info)
  "Check if there's already a SYMBOL containing INFO, unless `lsp-ui-sideline-ignore-duplicate'
is set to t."
  (not (when lsp-ui-sideline-ignore-duplicate
         (--any (and (string= (overlay-get it 'symbol) symbol)
                     (string= (overlay-get it 'info) info))
                lsp-ui-sideline--ovs))))

(defun lsp-ui-sideline--margin-width ()
  (+ (if fringes-outside-margins right-margin-width 0)
     (or (and (boundp 'fringe-mode)
              (consp fringe-mode)
              (or (equal (car fringe-mode) 0)
                  (equal (cdr fringe-mode) 0))
              1)
         (and (boundp 'fringe-mode) (equal fringe-mode 0) 1)
         0)
     (let ((win-fringes (window-fringes)))
       (if (or (equal (car win-fringes) 0)
               (equal (cadr win-fringes) 0))
           2
         0))
     (if (and (bound-and-true-p display-line-numbers-mode)
              (< emacs-major-version 27))
         ;; This was necessary with emacs < 27, recent versions take
         ;; into account the display-line width with :align-to
         (+ 2 (line-number-display-width))
       0)
     (if (or
          (bound-and-true-p whitespace-mode)
          (bound-and-true-p global-whitespace-mode))
         1
       0)))

(defun lsp-ui-sideline--window-width ()
  (- (min (window-text-width) (window-body-width))
     (lsp-ui-sideline--margin-width)
     (or (and (bound-and-true-p display-line-numbers-mode)
              (>= emacs-major-version 27)
              ;; We still need this number when calculating available space
              ;; even with emacs >= 27
              (+ (line-number-display-width) 2))
         0)))

(defun lsp-ui-sideline--display-all-info (buffer list-infos tag bol eol)
  (when (and (eq (current-buffer) buffer)
             (equal tag (lsp-ui-sideline--calculate-tag))
             (not (lsp-ui-sideline--stop-p)))
    (let ((inhibit-modification-hooks t)
          (win-width (window-body-width))
          ;; sort by bounds
          (list-infos (--sort (< (caadr it) (caadr other)) list-infos)))
      (lsp-ui-sideline--delete-kind 'info)
      (--each list-infos
        (-let (((symbol bounds info) it))
          (lsp-ui-sideline--push-info win-width symbol bounds info bol eol))))))

(defun lsp-ui-sideline--push-info (win-width symbol bounds info bol eol)
  (let* ((info (-some--> (lsp:hover-contents info)
                 (lsp-ui-sideline--extract-info it)
                 (lsp-ui-sideline--format-info it win-width)))
         (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
    (when (and (> (length info) 0)
               (lsp-ui-sideline--check-duplicate symbol info))
      (let* ((final-string (lsp-ui-sideline--make-display-string info symbol current))
             (pos-ov (lsp-ui-sideline--find-line (length final-string) bol eol))
             (ov (when pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
        (when pos-ov
          (overlay-put ov 'info info)
          (overlay-put ov 'symbol symbol)
          (overlay-put ov 'bounds bounds)
          (overlay-put ov 'current current)
          (overlay-put ov 'after-string final-string)
          (overlay-put ov 'before-string " ")
          (overlay-put ov 'window (get-buffer-window))
          (overlay-put ov 'kind 'info)
          (overlay-put ov 'position (car pos-ov))
          (push ov lsp-ui-sideline--ovs))))))

(defun lsp-ui-sideline--toggle-current (ov current)
  "Toggle the OV face according to CURRENT."
  (let* ((info (overlay-get ov 'info))
         (symbol (overlay-get ov 'symbol))
         (string (lsp-ui-sideline--make-display-string info symbol current)))
    (overlay-put ov 'current current)
    (overlay-put ov 'after-string string)))

(defun lsp-ui-sideline--highlight-current (point)
  "Update the symbol's face according to POINT."
  (dolist (ov lsp-ui-sideline--ovs)
    (let* ((bounds (overlay-get ov 'bounds))
           (start (car bounds))
           (end (cdr bounds)))
      (if (and bounds (>= point start) (<= point end))
          (unless (overlay-get ov 'current)
            (lsp-ui-sideline--toggle-current ov t))
        (when (overlay-get ov 'current)
          (lsp-ui-sideline--toggle-current ov nil))))))

(defun lsp-ui-sideline--split-long-lines (lines)
  "Fill LINES so that they are not longer than `lsp-ui-sideline-diagnostic-max-line-length' characters."
  (cl-mapcan (lambda (line)
               (if (< (length line) lsp-ui-sideline-diagnostic-max-line-length)
                   (list line)
                 (with-temp-buffer
                   (let ((fill-column lsp-ui-sideline-diagnostic-max-line-length))
                     (insert line)
                     (fill-region (point-min) (point-max))
                     (split-string (buffer-string) "\n")))))
             lines))

(defun lsp-ui-sideline--diagnostics (buffer bol eol)
  "Show diagnostics belonging to the current line.
Loop over flycheck errors with `flycheck-overlay-errors-in'.
Find appropriate position for sideline overlays with `lsp-ui-sideline--find-line'.
Push sideline overlays on `lsp-ui-sideline--ovs'."
  (when (and (bound-and-true-p flycheck-mode)
             (bound-and-true-p lsp-ui-sideline-mode)
             lsp-ui-sideline-show-diagnostics
             (eq (current-buffer) buffer))
    (lsp-ui-sideline--delete-kind 'diagnostics)
    (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
      (let* ((lines (--> (flycheck-error-format-message-and-id e)
                         (split-string it "\n")
                         (lsp-ui-sideline--split-long-lines it)))
             (display-lines (butlast lines (- (length lines) lsp-ui-sideline-diagnostic-max-lines)))
             (offset 1))
        (dolist (line (nreverse display-lines))
          (let* ((message (string-trim (replace-regexp-in-string "[\t ]+" " " line)))
                 (len (length message))
                 (level (flycheck-error-level e))
                 (face (if (eq level 'info) 'success level))
                 (margin (lsp-ui-sideline--margin-width))
                 (message (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil message)
                                 (add-face-text-property 0 len face nil message)
                                 message))
                 (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
                                 (propertize message 'display (lsp-ui-sideline--compute-height))))
                 (pos-ov (lsp-ui-sideline--find-line len bol eol t offset))
                 (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
            (when pos-ov
              (setq offset (1+ (car (cdr pos-ov))))
              (overlay-put ov 'after-string string)
              (overlay-put ov 'kind 'diagnostics)
              (overlay-put ov 'before-string " ")
              (overlay-put ov 'position (car pos-ov))
              (push ov lsp-ui-sideline--ovs))))))))

(defun lsp-ui-sideline-apply-code-actions nil
  "Choose and apply code action(s) on the current line."
  (interactive)
  (unless lsp-ui-sideline--code-actions
    (user-error "No code actions on the current line"))
  (lsp-execute-code-action (lsp--select-action lsp-ui-sideline--code-actions)))

(defun lsp-ui-sideline--scale-lightbulb (height)
  (--> (frame-char-height)
       (/ (float it) height)))

(defun lsp-ui-sideline--code-actions-make-image nil
  (let ((is-default (equal lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)))
    (--> `(image :type png :file ,lsp-ui-sideline-actions-icon :ascent center)
         (append it `(:scale ,(->> (cond (is-default 128)
                                         ((fboundp 'image-size) (cdr (image-size it t)))
                                         (t (error "Function image-size undefined.  Use default icon")))
                                   (lsp-ui-sideline--scale-lightbulb)))))))

(defun lsp-ui-sideline--code-actions-image nil
  (when lsp-ui-sideline-actions-icon
    (with-demoted-errors "[lsp-ui-sideline]: Error with actions icon: %s"
      (concat
       (propertize " " 'display (lsp-ui-sideline--code-actions-make-image))
       (propertize " " 'display '(space :width 0.3))))))

(defun lsp-ui-sideline--code-actions (actions bol eol)
  "Show code ACTIONS."
  (let ((inhibit-modification-hooks t))
    (when lsp-ui-sideline-actions-kind-regex
      (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
                                  (or (not kind?)
                                      (s-match lsp-ui-sideline-actions-kind-regex kind?)))
                                actions)))
    (setq lsp-ui-sideline--code-actions actions)
    (lsp-ui-sideline--delete-kind 'actions)
    (seq-doseq (action actions)
      (-let* ((title (->> (lsp:code-action-title action)
                          (replace-regexp-in-string "[\n\t ]+" " ")
                          (concat (unless lsp-ui-sideline-actions-icon
                                    lsp-ui-sideline-code-actions-prefix))))
              (image (lsp-ui-sideline--code-actions-image))
              (margin (lsp-ui-sideline--margin-width))
              (keymap (let ((map (make-sparse-keymap)))
                        (define-key map [down-mouse-1] (lambda () (interactive)
                                                         (save-excursion
                                                           (lsp-execute-code-action action))))
                        map))
              (len (length title))
              (title (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil title)
                            (add-face-text-property 0 len 'lsp-ui-sideline-code-action nil title)
                            (add-text-properties 0 len `(keymap ,keymap mouse-face highlight) title)
                            title))
              (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align (+ len (length image)) margin))))
                              image
                              (propertize title 'display (lsp-ui-sideline--compute-height))))
              (pos-ov (lsp-ui-sideline--find-line (+ 1 (length title) (length image)) bol eol t))
              (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
        (when pos-ov
          (overlay-put ov 'after-string string)
          (overlay-put ov 'before-string " ")
          (overlay-put ov 'kind 'actions)
          (overlay-put ov 'position (car pos-ov))
          (push ov lsp-ui-sideline--ovs))))))

(defun lsp-ui-sideline--calculate-tag nil
  "Calculate the tag used to determine whether to update sideline information."
  (cons (line-number-at-pos) (point)))

(defun lsp-ui-sideline--delete-kind (kind)
  (->> (--remove
        (when (eq (overlay-get it 'kind) kind)
          (--> (overlay-get it 'position)
               (remq it lsp-ui-sideline--occupied-lines)
               (setq lsp-ui-sideline--occupied-lines it))
          (delete-overlay it)
          t)
        lsp-ui-sideline--ovs)
       (setq lsp-ui-sideline--ovs)))

(defvar-local lsp-ui-sideline--last-tick-info nil)
(defvar-local lsp-ui-sideline--previous-line nil)

(defun lsp-ui-sideline--get-line (bol eol)
  (buffer-substring-no-properties bol eol))

(defun lsp-ui-sideline--run (&optional buffer bol eol this-line)
  "Show information (flycheck + lsp).
It loops on the symbols of the current line and requests information
from the language server."
  (when buffer-file-name
    (let* ((buffer (or buffer (current-buffer)))
           (eol (or eol (line-end-position)))
           (bol (or bol (line-beginning-position)))
           (tag (lsp-ui-sideline--calculate-tag))
           (line (car tag))
           (line-widen (if (buffer-narrowed-p) (save-restriction (widen) (line-number-at-pos)) line))
           (this-tick (buffer-modified-tick))
           (line-changed (not (equal line (car lsp-ui-sideline--tag))))
           (new-tick (unless line-changed (not (equal this-tick lsp-ui-sideline--last-tick-info))))
           (this-line (or this-line (lsp-ui-sideline--get-line bol eol)))
           (line-modified (and new-tick (not (equal this-line lsp-ui-sideline--previous-line))))
           (doc-id (lsp--text-document-identifier))
           (inhibit-modification-hooks t)
           symbols)
      (setq lsp-ui-sideline--tag tag
            lsp-ui-sideline--last-width (window-text-width))
      (when (and line-changed lsp-ui-sideline-show-diagnostics)
        (lsp-ui-sideline--diagnostics buffer bol eol))
      (when (and lsp-ui-sideline-show-code-actions
                 (or (lsp--capability "codeActionProvider")
                     (lsp--registered-capability "textDocument/codeAction")))
        (lsp-request-async
         "textDocument/codeAction"
         (if (eq lsp-ui-sideline-update-mode 'line)
             (list :textDocument doc-id
                   :range (lsp--region-to-range bol eol)
                   :context (list :diagnostics (lsp-cur-line-diagnostics)))
           (lsp--text-document-code-action-params))
         (lambda (actions)
           (when (eq (current-buffer) buffer)
             (lsp-ui-sideline--code-actions actions bol eol)))
         :mode 'tick
         :error-handler
         (lambda (&rest _)
           (lsp-ui-sideline--delete-kind 'actions))
         :cancel-token :lsp-ui-code-actions))
      ;; Go through all symbols and request hover information.  Note that the symbols are
      ;; traversed backwards as `forward-symbol' with a positive argument will jump just past the
      ;; current symbol.  By going from the end of the line towards the front, point will be placed
      ;; at the beginning of each symbol.  As the requests are first collected in a list before
      ;; being processed they are still sent in order from left to right.
      (when (and lsp-ui-sideline-show-hover (or line-changed line-modified) (lsp--capability "hoverProvider"))
        (setq lsp-ui-sideline--last-tick-info this-tick
              lsp-ui-sideline--previous-line this-line)
        (save-excursion
          (goto-char eol)
          (while (and (> (point) bol)
                      (progn (forward-symbol -1)
                             (>= (point) bol)))
            (let* ((symbol (thing-at-point 'symbol t))
                   (bounds (bounds-of-thing-at-point 'symbol))
                   (parsing-state (syntax-ppss))
                   (in-string (nth 3 parsing-state))
                   (outside-comment (eq (nth 4 parsing-state) nil)))
              ;; Skip strings and comments
              (when (and symbol (not in-string) outside-comment)
                (push (list symbol bounds (list :line (1- line-widen) :character (- (point) bol))) symbols))))
          (if (null symbols)
              (lsp-ui-sideline--delete-kind 'info)
            (let ((length-symbols (length symbols))
                  (current-index 0)
                  list-infos)
              (--each symbols
                (-let (((symbol bounds position) it))
                  (lsp-request-async
                   "textDocument/hover"
                   (lsp-make-hover-params :text-document doc-id :position position)
                   (lambda (info)
                     (setq current-index (1+ current-index))
                     (and info (push (list symbol bounds info) list-infos))
                     (when (or (= current-index length-symbols) (not lsp-ui-sideline-wait-for-all-symbols))
                       (lsp-ui-sideline--display-all-info buffer list-infos tag bol eol)))
                   :error-handler
                   (lambda (&rest _)
                     (setq current-index (1+ current-index))
                     (when (or (= current-index length-symbols) (not lsp-ui-sideline-wait-for-all-symbols))
                       (lsp-ui-sideline--display-all-info buffer list-infos tag bol eol)))
                   :mode 'tick))))))))))

(defun lsp-ui-sideline--stop-p ()
  "Return non-nil if the sideline should not be display."
  (or (region-active-p)
      (bound-and-true-p company-pseudo-tooltip-overlay)
      (bound-and-true-p lsp-ui-peek--overlay)))

(defun lsp-ui-sideline--hide-before-company (command)
  "Disable the sideline before company's overlay appears.
COMMAND is `company-pseudo-tooltip-frontend' parameter."
  (when (memq command '(post-command update))
    (lsp-ui-sideline--delete-ov)))

(defun lsp-ui-sideline ()
  "Show information for the current line."
  (if (lsp-ui-sideline--stop-p)
      (lsp-ui-sideline--delete-ov)
    (let* ((current-line (line-number-at-pos))
           (same-line (equal current-line (car lsp-ui-sideline--tag)))
           (same-width (equal (window-text-width) lsp-ui-sideline--last-width))
           (new-tick (and same-line (not (equal (buffer-modified-tick) lsp-ui-sideline--last-tick-info))))
           (bol (and new-tick (line-beginning-position)))
           (eol (and new-tick (line-end-position)))
           (this-line (and new-tick (lsp-ui-sideline--get-line bol eol)))
           (unmodified (if new-tick (equal this-line lsp-ui-sideline--previous-line) t))
           (buffer (current-buffer))
           (point (point)))
      (cond ((and unmodified same-line same-width)
             (lsp-ui-sideline--highlight-current (point)))
            ((not (and same-line same-width))
             (lsp-ui-sideline--delete-ov)))
      (when lsp-ui-sideline--timer
        (cancel-timer lsp-ui-sideline--timer))
      (setq lsp-ui-sideline--timer
            (run-with-idle-timer
             lsp-ui-sideline-delay nil
             (lambda nil
               ;; run lsp-ui only if current-buffer is the same.
               (and (eq buffer (current-buffer))
                    (= point (point))
                    (lsp-ui-sideline--run buffer bol eol this-line))))))))

(defun lsp-ui-sideline-toggle-symbols-info ()
  "Toggle display of symbols information.
This does not toggle display of flycheck diagnostics or code actions."
  (interactive)
  (when (bound-and-true-p lsp-ui-sideline-mode)
    (setq lsp-ui-sideline-show-hover
          (not lsp-ui-sideline-show-hover))
    (lsp-ui-sideline--run (current-buffer))))

(defun lsp-ui-sideline--diagnostics-changed ()
  "Handler for flycheck notifications."
  (when lsp-ui-sideline-show-diagnostics
    (let* ((buffer (current-buffer))
           (eol (line-end-position))
           (bol (line-beginning-position)))
      (lsp-ui-sideline--diagnostics buffer bol eol))))

(defun lsp-ui-sideline--erase (&rest _)
  "Remove all sideline overlays and delete last tag."
  (when (bound-and-true-p lsp-ui-sideline-mode)
    (ignore-errors
      (lsp-ui-sideline--delete-ov))))

(define-minor-mode lsp-ui-sideline-mode
  "Minor mode for showing information for current line."
  :init-value nil
  :group lsp-ui-sideline
  (cond
   (lsp-ui-sideline-mode
    (add-hook 'post-command-hook 'lsp-ui-sideline nil t)
    (advice-add 'company-pseudo-tooltip-frontend :before 'lsp-ui-sideline--hide-before-company)
    (add-hook 'flycheck-after-syntax-check-hook 'lsp-ui-sideline--diagnostics-changed nil t)
    (when lsp-ui-sideline-show-diagnostics
      (setq-local flycheck-display-errors-function nil)))
   (t
    (advice-remove 'company-pseudo-tooltip-frontend 'lsp-ui-sideline--hide-before-company)
    (lsp-ui-sideline--delete-ov)
    (remove-hook 'flycheck-after-syntax-check-hook  'lsp-ui-sideline--diagnostics-changed t)
    (remove-hook 'post-command-hook 'lsp-ui-sideline t)
    (when lsp-ui-sideline-show-diagnostics
      (kill-local-variable 'flycheck-display-errors-function)))))

(defun lsp-ui-sideline-enable (enable)
  "Enable/disable `lsp-ui-sideline-mode'."
  (lsp-ui-sideline-mode (if enable 1 -1))
  (if enable
      (add-hook 'before-revert-hook 'lsp-ui-sideline--delete-ov nil t)
    (remove-hook 'before-revert-hook 'lsp-ui-sideline--delete-ov t)))

(provide 'lsp-ui-sideline)
;;; lsp-ui-sideline.el ends here
