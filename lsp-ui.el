;;; lsp-ui.el --- UI modules for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Sebastien Chapuis, Fangrui Song

;; Author: Sebastien Chapuis <sebastien@chapu.is>, Fangrui Song <i@maskray.me>
;; Keywords: languages, tools
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Package-Requires: ((emacs "26.1") (dash "2.18.0") (lsp-mode "6.0") (markdown-mode "2.3"))
;; Version: 8.0.0

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

;; lsp-ui contains a series of useful UI integrations for lsp-mode, like
;; flycheck support and code lenses.

;;; Code:

(require 'dash)
(require 'lsp-protocol)
(require 'find-func)

(defconst lsp-ui-resources-dir
  (--> (or load-file-name (buffer-file-name))
       (file-name-directory it)
       (expand-file-name "resources" it)
       (file-name-as-directory it)
       (and (file-directory-p it) it))
  "Resource folder for package `lsp-ui'.")

(defgroup lsp-ui nil
  "‘lsp-ui’ contains a series of useful UI integrations for ‘lsp-mode’."
  :group 'tools
  :group 'convenience
  :link '(custom-manual "(lsp-ui) Top")
  :link '(info-link "(lsp-ui) Customizing"))

(with-eval-after-load 'flycheck
  (require 'lsp-ui-flycheck))

(with-eval-after-load 'winum
  (when (and (boundp 'winum-ignored-buffers-regexp) lsp-ui-doc-winum-ignore)
    (add-to-list 'winum-ignored-buffers-regexp lsp-ui-doc--buffer-prefix)))

(defun lsp-ui--workspace-path (path)
  "Return the PATH relative to the workspace.
If the PATH is not in the workspace, it returns the original PATH."
  (let* ((path (file-truename path))
         (root (lsp-workspace-root path))
         (in-workspace (and root (string-prefix-p root path))))
    (if in-workspace
        (substring path (length root))
      path)))

(defun lsp-ui--toggle (enable)
  (dolist (feature '(lsp-ui-peek lsp-ui-sideline lsp-ui-doc lsp-ui-imenu))
    (let* ((sym (--> (intern-soft (concat (symbol-name feature) "-enable"))
                     (and (boundp it) it)))
           (value (symbol-value sym))
           (fn (symbol-function sym)))
      (and (or value (not enable))
           (functionp fn)
           (funcall fn enable)))))

(defvar lsp-ui-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode lsp-ui-mode
  "Toggle language server UI mode on or off.
‘lsp-ui-mode’ is a minor mode that contains a series of useful UI
integrations for ‘lsp-mode’.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is ‘toggle’."
  :init-value nil
  :group lsp-ui
  :keymap lsp-ui-mode-map
  (lsp-ui--toggle lsp-ui-mode))

;; The request is delegated to xref-backend-apropos defined in lsp-mode.
;; xref-find-apropos does similar job but is less appealing because it splits and
;; regex quotes the pattern. The language server likely knows more about how
;; to do fuzzy matching.
(defun lsp-ui-find-workspace-symbol (pattern)
  "List project-wide symbols matching the query string PATTERN."
  (interactive (list (read-string
                      "workspace/symbol: "
                      nil 'xref--read-pattern-history)))
  (xref--find-xrefs pattern 'apropos pattern nil))

(defun lsp-ui--location< (x y)
  "Compares two triples X and Y.
Both should have the form (FILENAME LINE COLUMN)."
  (if (not (string= (car x) (car y)))
      (string< (car x) (car y))
    (if (not (= (cadr x) (cadr y)))
        (< (cadr x) (cadr y))
      (< (caddr x) (caddr y)))))

(defun lsp-ui--reference-triples (include-declaration)
  "Return references as a list of (FILENAME LINE COLUMN) triples given EXTRA."
  (let ((refs (lsp-request "textDocument/references"
                           (lsp--make-reference-params nil include-declaration))))
    (sort
     (mapcar
      (-lambda ((&Location :uri :range (&Range :start (&Position :line :character))))
        (list (lsp--uri-to-path uri) line character))
      refs)
     #'lsp-ui--location<)))

;; TODO Make it efficient
(defun lsp-ui-find-next-reference (&optional include-declaration)
  "Find next reference of the symbol at point."
  (interactive)
  (let* ((cur (list buffer-file-name (1- (line-number-at-pos)) (- (point) (line-beginning-position))))
         (refs (lsp-ui--reference-triples include-declaration))
         (idx -1)
         (res (-first (lambda (ref) (cl-incf idx) (lsp-ui--location< cur ref)) refs)))
    (if res
        (progn
          (find-file (car res))
          (goto-char 1)
          (forward-line (cadr res))
          (forward-char (caddr res))
          (cons idx (length refs)))
      (cons 0 0))))

;; TODO Make it efficient
(defun lsp-ui-find-prev-reference (&optional include-declaration)
  "Find previous reference of the symbol at point."
  (interactive)
  (let* ((cur (list buffer-file-name (1- (line-number-at-pos)) (- (point) (line-beginning-position))))
         (refs (lsp-ui--reference-triples include-declaration))
         (idx -1)
         (res (-last (lambda (ref) (and (lsp-ui--location< ref cur) (cl-incf idx))) refs)))
    (if res
        (progn
          (find-file (car res))
          (goto-char 1)
          (forward-line (cadr res))
          (forward-char (caddr res))
          (cons idx (length refs)))
      (cons 0 0))))

;;
;;; Util

(defmacro lsp-ui--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defmacro lsp-ui--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         after-focus-change-function)
     ,@body))

(defun lsp-ui-safe-kill-timer (timer)
  "Safely kill the TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun lsp-ui-safe-delete-overlay (overlay)
  "Safely delete the OVERLAY."
  (when (overlayp overlay) (delete-overlay overlay)))

(defun lsp-ui-line-number-display-width ()
  "Safe way to get value from function `line-number-display-width'."
  (if (bound-and-true-p display-line-numbers-mode)
      ;; For some reason, function `line-number-display-width' gave
      ;; us error `args-out-of-range' even we do not pass anything towards
      ;; to it function. See the following links,
      ;;
      ;; - https://github.com/emacs-lsp/lsp-ui/issues/294
      ;; - https://github.com/emacs-lsp/lsp-ui/issues/533 (duplicate)
      (+ (or (ignore-errors (line-number-display-width)) 0) 2)
    0))

;;
;;; Core

(require 'lsp-ui-sideline)
(require 'lsp-ui-peek)
(require 'lsp-ui-imenu)
(require 'lsp-ui-doc)

(provide 'lsp-ui)
;;; lsp-ui.el ends here
