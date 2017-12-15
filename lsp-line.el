;;; lsp-line.el --- Lsp-Line  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (flycheck "0.23") (lsp-mode "3.4"))

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

(defgroup lsp-line nil
  "Display informations of the current line."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-line) Top")
  :link '(info-link "(lsp-line) Customizing"))

(defcustom lsp-line-enable t
  "Whether or not to enable lsp-line."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-line-ignore-duplicate nil
  "Control to ignore duplicates when there is a same symbol with the same contents."
  :type 'boolean
  :group 'lsp-ui)

(defvar lsp-line-code-actions-prefix
  (propertize "💡 " 'face '(:foreground "yellow"))
  "Prefix to insert before the code action title.")

(defvar-local lsp-line--ovs nil
  "Overlays used by `lsp-line'.")

(defvar-local lsp-line--occupied-lines nil
  "List of lines occupied by an overlay of `lsp-line'.")

(defvar-local lsp-line--line nil
  "Line where the last operation was based.
It is used to know when the cursor has changed of line.")

(defvar-local lsp-line--last-width nil
  "Value of window's width on the last operation.
It is used to know when the window has changed of width.")

(defvar-local lsp-line--timer nil)

(defface lsp-line-symbol
  '((t :foreground "grey"
       :box (:line-width -1 :color "grey")
       :height 0.99))
  "Face used to highlight symbols."
  :group 'lsp-line)

(defface lsp-line-current-symbol
  '((t :foreground "white"
       :weight ultra-bold
       :box (:line-width -1 :color "white")
       :height 0.99))
  "Face used to highlight the symbol on point."
  :group 'lsp-line)

(defun lsp-line--calc-space (win-width str-len index)
  "Calcul whether there is enough space on line.
If there is enough space, it returns the point of the last
character on the line.

WIN-WIDTH is the window width.
STR-LEN is the string size.
INDEX is the line number (relative to the current line)."
  (let ((eol (line-end-position index)))
    (unless (member eol lsp-line--occupied-lines)
      (save-excursion
        (goto-char eol)
        (when (>= (- win-width (current-column)) str-len)
          eol)))))

(defun lsp-line--find-line (win-width str-len &optional up)
  "Find a line where the string can be inserted.
It loops on the nexts lines to find enough space.
Returns the point of the last character on the line.

WIN-WIDTH is the window width.
STR-LEN is the string size.
if UP is non-nil, it loops on the previous lines.."
  (let (pos (index 1))
    (while (and (null pos) (<= (abs index) 30))
      (setq index (if up (1- index) (1+ index)))
      (setq pos (lsp-line--calc-space win-width str-len index)))
    (when pos (push pos lsp-line--occupied-lines))
    pos))

(defun lsp-line--delete-ov ()
  "Delete overlays."
  (seq-do 'delete-overlay lsp-line--ovs)
  (setq lsp-line--ovs nil))

(defun lsp-line--get-renderer (language)
  "Return a function to fontify a string in LANGUAGE."
  (thread-last lsp--cur-workspace
    lsp--workspace-client
    lsp--client-string-renderers
    (assoc-string language)
    cdr))

(defun lsp-line--get-language ()
  "Return the language of the buffer."
  (thread-first lsp--cur-workspace
    lsp--workspace-client
    lsp--client-language-id
    (funcall (current-buffer))))

(defun lsp-line--extract-info (contents)
  "Extract the line to print from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We prioritize string with a language (which is probably a type or a
function signature)."
  (when contents
    (let* ((strings (when (listp contents) (seq-group-by 'hash-table-p contents)))
           (string (alist-get nil strings))
           (strings-with-language (alist-get t strings)))
      (or (when (stringp contents) contents)
          (when (hash-table-p contents) contents)
          (when (listp strings-with-language)
            (or (car (seq-filter (lambda (s) (string= (gethash "language" s)
                                                      (lsp-line--get-language)))
                                 strings-with-language))
                (car strings-with-language)))
          strings-with-language
          (when (listp string) (car string))
          string))))

(defun lsp-line--format-info (marked-string)
  "Format MARKED-STRING.
If the string has a language, we fontify it with the function provided
by `lsp-mode'.
MARKED-STRING is the string returned by `lsp-line--extract-info'."
  (when marked-string
    (when (hash-table-p marked-string)
      (let* ((language (gethash "language" marked-string))
             (value (gethash "value" marked-string))
             (renderer (lsp-line--get-renderer language)))
        (setq marked-string (if (and (functionp renderer) value)
                                (funcall renderer value)
                              value))))
    (add-face-text-property 0 (length marked-string) '(:slant italic :height 0.99) nil marked-string)
    (subst-char-in-string ?\n ?\s marked-string t)))

(defun lsp-line--make-display-string (info symbol current)
  "Make final string to display on buffer.
INFO is the information to display.
SYMBOL is the symbol associated to the info.
CURRENT is non-nil when the point is on the symbol."
  (let* ((str (concat info " " (propertize (concat " " symbol " ")
                                           'face (if current 'lsp-line-current-symbol 'lsp-line-symbol))))
         (len (length str)))
    (concat
     (propertize " " 'display `(space :align-to (- right-fringe ,(+ 1 (length str)))))
     str)))

(defun lsp-line--check-duplicate (symbol info)
  "SYMBOL INFO."
  (not (when lsp-line-ignore-duplicate
         (--any (and (string= (overlay-get it 'symbol) symbol)
                     (string= (overlay-get it 'info) info))
                lsp-line--ovs))))

(defun lsp-line--push-info (symbol line bounds info)
  "SYMBOL LINE BOUNDS INFO."
  (when (= line (line-number-at-pos))
    (let* ((info (concat (thread-first (gethash "contents" info)
                           lsp-line--extract-info
                           lsp-line--format-info)))
           (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
      (when (and (> (length info) 0)
                 (lsp-line--check-duplicate symbol info))
        (let* ((final-string (lsp-line--make-display-string info symbol current))
               (pos-ov (lsp-line--find-line (window-text-width) (length final-string)))
               (ov (when pos-ov (make-overlay pos-ov pos-ov))))
          (when pos-ov
            (overlay-put ov 'info info)
            (overlay-put ov 'symbol symbol)
            (overlay-put ov 'bounds bounds)
            (overlay-put ov 'current current)
            (overlay-put ov 'after-string final-string)
            (overlay-put ov 'window (get-buffer-window))
            (push ov lsp-line--ovs)))))))

(defun lsp-line--toggle-current (ov current)
  "Toggle the OV face according to CURRENT."
  (let* ((info (overlay-get ov 'info))
         (symbol (overlay-get ov 'symbol))
         (string (lsp-line--make-display-string info symbol current)))
    (overlay-put ov 'current current)
    (overlay-put ov 'after-string string)))

(defun lsp-line--highlight-current (point)
  "Update the symbol's face according to POINT."
  (dolist (ov lsp-line--ovs)
    (let* ((bounds (overlay-get ov 'bounds))
           (start (car bounds))
           (end (cdr bounds)))
      (if (and bounds (>= point start) (<= point end))
          (unless (overlay-get ov 'current)
            (lsp-line--toggle-current ov t))
        (when (overlay-get ov 'current)
          (lsp-line--toggle-current ov nil))))))

(defun lsp-line--flycheck ()
  "Show flycheck message(s)."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
      (let* ((message (flycheck-error-format-message-and-id e))
	     (level (flycheck-error-level e))
	     (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length message)))))
                             (propertize message 'face (pcase level
                                                         ('error 'error)
                                                         ('warning 'warning)
                                                         (_ 'success)))))
             (pos-ov (lsp-line--find-line (window-text-width) (length message) t))
             (ov (make-overlay pos-ov pos-ov)))
        (when pos-ov
          (overlay-put ov 'after-string string)
          (push ov lsp-line--ovs))))))

(defun lsp-line--code-actions (actions)
  "Show code ACTIONS."
  (dolist (action actions)
    (-let* (((&hash "title" title) action)
            (title (concat lsp-line-code-actions-prefix title))
            (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length title)))))
                            title))
            (pos-ov (lsp-line--find-line (window-text-width) (length title) t))
            (ov (and pos-ov (make-overlay pos-ov pos-ov))))
      (when pos-ov
        (overlay-put ov 'after-string string)
        (push ov lsp-line--ovs)))))

(defun lsp-line--run ()
  "Show informations (flycheck + lsp).
It loops on the symbols of the current line and request information
to the language server."
  (lsp-line--delete-ov)
  (lsp-line--flycheck)
  (when lsp--cur-workspace
    (let ((eol (line-end-position))
          (eob (buffer-end 1))
          (bol (line-beginning-position))
          (line (line-number-at-pos))
          (doc-id (lsp--text-document-identifier)))
      (save-excursion
        (goto-char bol)
        (setq lsp-line--occupied-lines nil
              lsp-line--line line
              lsp-line--last-width (window-text-width))
        (lsp--send-request-async (lsp--make-request
                                  "textDocument/codeAction"
                                  (list :textDocument (lsp--text-document-identifier)
                                        :range (lsp--region-to-range bol eol)
                                        :context (list :diagnostics (lsp--cur-line-diagnotics))))
                                 #'lsp-line--code-actions)
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
               (lambda (info) (lsp-line--push-info symbol line bounds info))))
            (forward-symbol 1)))))))

(defun lsp-line ()
  "Show informations of the current line."
  (if (or (region-active-p)
          (bound-and-true-p company-pseudo-tooltip-overlay)
          (bound-and-true-p lsp-xref--peek-overlay))
      (progn
        (setq lsp-line--line nil)
        (lsp-line--delete-ov))
    (if (and (equal (line-number-at-pos) lsp-line--line)
             (equal (window-text-width) lsp-line--last-width))
        (lsp-line--highlight-current (point))
      (lsp-line--delete-ov)
      (when lsp-line--timer
        (cancel-timer lsp-line--timer))
      (setq lsp-line--timer
            (run-with-idle-timer 0.2 nil 'lsp-line--run)))))

(define-minor-mode lsp-line-mode
  "Minor mode for showing information of current line."
  :init-value nil
  :group lsp-line
  (cond
   (lsp-line-mode
    (add-hook 'post-command-hook 'lsp-line nil t)
    (setq-local flycheck-display-errors-function nil))
   (t
    (setq lsp-line--line nil)
    (lsp-line--delete-ov)
    (remove-hook 'post-command-hook 'lsp-line t))
   ))

(defun lsp-line-enable (enable)
  "Enable/disable lsp-line-mode."
  (if enable
      (lsp-line-mode 1)
    (lsp-line-mode -1)))

(provide 'lsp-line)
;;; lsp-line.el ends here
