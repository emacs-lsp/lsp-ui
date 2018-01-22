;;; lsp-ui-sideline.el --- Lsp-Ui-Sideline  -*- lexical-binding: t -*-

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
;; Utility to show informations of the current line

;;; Code:

(require 'lsp-mode)
(require 'flycheck)
(require 'dash)

(defgroup lsp-ui-sideline nil
  "Display informations of the current line."
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
  "Control to ignore duplicates when there is a same symbol with the same contents."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-symbol t
  "Whether to show the symbol on the right of the information."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-hover t
  "Whether to show hover messages in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-flycheck t
  "Whether to show flycheck messages in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-show-code-actions t
  "Whether to show code actions in sideline."
  :type 'boolean
  :group 'lsp-ui-sideline)

(defcustom lsp-ui-sideline-delay 0.2
  "Number of seconds to wait before showing sideline."
  :type 'number
  :group 'lsp-ui-sideline)

(defvar lsp-ui-sideline-code-actions-prefix
  (propertize "💡 " 'face '(:foreground "yellow"))
  "Prefix to insert before the code action title.")

(defvar-local lsp-ui-sideline--ovs nil
  "Overlays used by `lsp-ui-sideline'.")

(defvar-local lsp-ui-sideline--occupied-lines nil
  "List of lines occupied by an overlay of `lsp-ui-sideline'.")

(defvar-local lsp-ui-sideline--line nil
  "Line where the last operation was based.
It is used to know when the cursor has changed of line.")

(defvar-local lsp-ui-sideline--last-width nil
  "Value of window's width on the last operation.
It is used to know when the window has changed of width.")

(defvar-local lsp-ui-sideline--timer nil)

(defface lsp-ui-sideline-symbol
  '((t :foreground "grey"
       :box (:line-width -1 :color "grey")
       :height 0.99))
  "Face used to highlight symbols."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-current-symbol
  '((t :foreground "white"
       :weight ultra-bold
       :box (:line-width -1 :color "white")
       :height 0.99))
  "Face used to highlight the symbol on point."
  :group 'lsp-ui-sideline)

(defface lsp-ui-sideline-code-action
  '((t :foreground "yellow"))
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

(defun lsp-ui-sideline--find-line (str-len &optional up)
  "Find a line where the string can be inserted.
It loops on the nexts lines to find enough space.
Returns the point of the last character on the line.

WIN-WIDTH is the window width.
STR-LEN is the string size.
if UP is non-nil, it loops on the previous lines.."
  (let ((win-width (lsp-ui-sideline--window-width))
        (index 1) pos)
    (while (and (null pos) (<= (abs index) 30))
      (setq index (if up (1- index) (1+ index)))
      (setq pos (lsp-ui-sideline--calc-space win-width str-len index)))
    (when pos (push pos lsp-ui-sideline--occupied-lines))
    (if (equal pos (point-min))
        (lsp-ui-sideline--find-line str-len)
      pos)))

(defun lsp-ui-sideline--delete-ov ()
  "Delete overlays."
  (seq-do 'delete-overlay lsp-ui-sideline--ovs)
  (setq lsp-ui-sideline--ovs nil))

(defun lsp-ui-sideline--get-renderer (language)
  "Return a function to fontify a string in LANGUAGE."
  (thread-last lsp--cur-workspace
    lsp--workspace-client
    lsp--client-string-renderers
    (assoc-string language)
    cdr))

(defun lsp-ui-sideline--get-language ()
  "Return the language of the buffer."
  (thread-first lsp--cur-workspace
    lsp--workspace-client
    lsp--client-language-id
    (funcall (current-buffer))))

(defun lsp-ui-sideline--extract-info (contents)
  "Extract the line to print from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We prioritize string with a language (which is probably a type or a
function signature)."
  (when contents
    (cond
     ((stringp contents) contents)
     ((listp contents) ;; MarkedString[]
      (--first (and (hash-table-p it)
                    (lsp-ui-sideline--get-renderer (gethash "language" it)))
               contents))
     ((gethash "kind" contents) (gethash "value" contents)) ;; MarkupContent
     ((gethash "language" contents) ;; MarkedString
      (and (lsp-ui-sideline--get-renderer (gethash "language" contents))
           (gethash "value" contents))))))

(defun lsp-ui-sideline--format-info (marked-string)
  "Format MARKED-STRING.
If the string has a language, we fontify it with the function provided
by `lsp-mode'.
MARKED-STRING is the string returned by `lsp-ui-sideline--extract-info'."
  (when marked-string
    (when (hash-table-p marked-string)
      (let* ((language (gethash "language" marked-string))
             (value (gethash "value" marked-string))
             (renderer (lsp-ui-sideline--get-renderer language)))
        (setq marked-string (if (and (functionp renderer) value)
                                (funcall renderer value)
                              value))))
    (add-face-text-property 0 (length marked-string) 'lsp-ui-sideline-symbol-info nil marked-string)
    (add-face-text-property 0 (length marked-string) 'default t marked-string)
    (replace-regexp-in-string "[\n\t]+" " " marked-string)))

(defun lsp-ui-sideline--make-display-string (info symbol current)
  "Make final string to display on buffer.
INFO is the information to display.
SYMBOL is the symbol associated to the info.
CURRENT is non-nil when the point is on the symbol."
  (let* ((face (if current 'lsp-ui-sideline-current-symbol 'lsp-ui-sideline-symbol))
         (str (if lsp-ui-sideline-show-symbol
                  (concat info " " (propertize (concat " " symbol " ") 'face face))
                info))
         (len (length str))
         (margin (lsp-ui-sideline--margin-width)))
    (add-face-text-property 0 len 'lsp-ui-sideline-global nil str)
    (concat
     (propertize " " 'display `(space :align-to (- right-fringe ,(+ 1 len margin))))
     str)))

(defun lsp-ui-sideline--check-duplicate (symbol info)
  (not (when lsp-ui-sideline-ignore-duplicate
         (--any (and (string= (overlay-get it 'symbol) symbol)
                     (string= (overlay-get it 'info) info))
                lsp-ui-sideline--ovs))))

(defun lsp-ui-sideline--margin-width ()
  (if fringes-outside-margins right-margin-width 0))

(defun lsp-ui-sideline--window-width ()
  (- (window-text-width)
     (lsp-ui-sideline--margin-width)))

(defun lsp-ui-sideline--push-info (symbol line bounds info)
  (when (= line (line-number-at-pos))
    (let* ((info (concat (thread-first (gethash "contents" info)
                           lsp-ui-sideline--extract-info
                           lsp-ui-sideline--format-info)))
           (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
      (when (and (> (length info) 0)
                 (lsp-ui-sideline--check-duplicate symbol info))
        (let* ((final-string (lsp-ui-sideline--make-display-string info symbol current))
               (pos-ov (lsp-ui-sideline--find-line (length final-string)))
               (ov (when pos-ov (make-overlay pos-ov pos-ov))))
          (when pos-ov
            (overlay-put ov 'info info)
            (overlay-put ov 'symbol symbol)
            (overlay-put ov 'bounds bounds)
            (overlay-put ov 'current current)
            (overlay-put ov 'after-string final-string)
            (overlay-put ov 'window (get-buffer-window))
            (push ov lsp-ui-sideline--ovs)))))))

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

(defun lsp-ui-sideline--flycheck ()
  "Show flycheck message(s)."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
      (let* ((message (->> (flycheck-error-format-message-and-id e)
                           (replace-regexp-in-string "[\n\t]+" " ")))
             (len (length message))
	     (level (flycheck-error-level e))
             (face (if (eq level 'info) 'success level))
             (margin (lsp-ui-sideline--margin-width))
             (message (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil message)
                             (add-face-text-property 0 len face nil message)
                             message))
             (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(+ 1 len margin))))
                             message))
             (pos-ov (lsp-ui-sideline--find-line len t))
             (ov (and pos-ov (make-overlay pos-ov pos-ov))))
        (when pos-ov
          (overlay-put ov 'after-string string)
          (push ov lsp-ui-sideline--ovs))))))

(defun lsp-ui-sideline--code-actions (actions)
  "Show code ACTIONS."
  (dolist (action actions)
    (-let* ((title (->> (gethash "title" action)
                        (replace-regexp-in-string "[\n\t]+" " ")
                        (concat lsp-ui-sideline-code-actions-prefix)))
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
            (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(+ 1 (length title) margin))))
                            title))
            (pos-ov (lsp-ui-sideline--find-line (length title) t))
            (ov (and pos-ov (make-overlay pos-ov pos-ov))))
      (when pos-ov
        (overlay-put ov 'after-string string)
        (push ov lsp-ui-sideline--ovs)))))

(defun lsp-ui-sideline--run ()
  "Show informations (flycheck + lsp).
It loops on the symbols of the current line and request information
to the language server."
  (lsp-ui-sideline--delete-ov)
  (when lsp--cur-workspace
    (let ((eol (line-end-position))
          (eob (buffer-end 1))
          (bol (line-beginning-position))
          (line (line-number-at-pos))
          (doc-id (lsp--text-document-identifier)))
      (save-excursion
        (goto-char bol)
        (setq lsp-ui-sideline--occupied-lines nil
              lsp-ui-sideline--line line
              lsp-ui-sideline--last-width (window-text-width))
        (when lsp-ui-sideline-show-flycheck
          (lsp-ui-sideline--flycheck))
        (when (and lsp-ui-sideline-show-code-actions (lsp--capability "codeActionProvider"))
          (lsp--send-request-async (lsp--make-request
                                    "textDocument/codeAction"
                                    (list :textDocument doc-id
                                          :range (lsp--region-to-range bol eol)
                                          :context (list :diagnostics (lsp--cur-line-diagnotics))))
                                   #'lsp-ui-sideline--code-actions))
        (when lsp-ui-sideline-show-hover
          (while (and (<= (point) eol) (< (point) eob))
            (let ((symbol (thing-at-point 'symbol t))
                  (bounds (bounds-of-thing-at-point 'symbol))
                  (column (lsp--cur-column)))
              (when symbol
                (lsp--send-request-async
                 (lsp--make-request
                  "textDocument/hover"
                  (list :textDocument doc-id
                        :position (lsp--position (1- line) (if (= column 0) 0 (1- column)))))
                 (lambda (info) (if info (lsp-ui-sideline--push-info symbol line bounds info)))))
              (forward-symbol 1))))))))

(defun lsp-ui-sideline ()
  "Show informations of the current line."
  (if (or (region-active-p)
          (bound-and-true-p company-pseudo-tooltip-overlay)
          (bound-and-true-p lsp-ui-peek--overlay))
      (progn
        (setq lsp-ui-sideline--line nil)
        (lsp-ui-sideline--delete-ov))
    (if (and (equal (line-number-at-pos) lsp-ui-sideline--line)
             (equal (window-text-width) lsp-ui-sideline--last-width))
        (lsp-ui-sideline--highlight-current (point))
      (lsp-ui-sideline--delete-ov)
      (when lsp-ui-sideline--timer
        (cancel-timer lsp-ui-sideline--timer))
      (setq lsp-ui-sideline--timer
            (run-with-idle-timer lsp-ui-sideline-delay nil 'lsp-ui-sideline--run)))))

(defun lsp-ui-sideline-toggle-symbols-info ()
  "Toggle display of symbols informations.
This does not toggle display of flycheck diagnostics or code actions."
  (interactive)
  (when (bound-and-true-p lsp-ui-sideline-mode)
    (setq lsp-ui-sideline-show-hover
          (not lsp-ui-sideline-show-hover))
    (lsp-ui-sideline--run)))

(define-minor-mode lsp-ui-sideline-mode
  "Minor mode for showing information of current line."
  :init-value nil
  :group lsp-ui-sideline
  (cond
   (lsp-ui-sideline-mode
    (add-hook 'post-command-hook 'lsp-ui-sideline nil t)
    (setq-local flycheck-display-errors-function nil))
   (t
    (setq lsp-ui-sideline--line nil)
    (lsp-ui-sideline--delete-ov)
    (remove-hook 'post-command-hook 'lsp-ui-sideline t))
   ))

(defun lsp-ui-sideline-enable (enable)
  "Enable/disable ‘lsp-ui-sideline-mode’."
  (lsp-ui-sideline-mode (if enable 1 -1)))

(provide 'lsp-ui-sideline)
;;; lsp-ui-sideline.el ends here
