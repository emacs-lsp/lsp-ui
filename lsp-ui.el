;;; lsp-ui.el --- UI modules for lsp-mode            -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Author:  Tobias Pisani <topisani@hamsterpoison.com>
;; Keywords: lsp
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Package-Requires: ((emacs "25.1") (flycheck "30") (lsp-mode "3.4") (markdown-mode "2.0"))
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; lsp-ui contains a series of useful UI integrations for lsp-mode, like
;; flycheck support and code lenses.

;;; Code:

(defgroup lsp-ui nil
  "lsp-ui contains a series of useful UI integrations for lsp-mode."
  :group 'tools
  :group 'convenience
  :link '(custom-manual "(lsp-ui) Top")
  :link '(info-link "(lsp-ui) Customizing"))

(require 'lsp-ui-sideline)
(require 'lsp-ui-peek)
(require 'lsp-ui-flycheck)
(require 'lsp-ui-imenu)

;; Child frames are only available since Emacs 26, so don’t require
;; ‘lsp-ui-doc’ unconditionally.
(when (fboundp 'display-buffer-in-child-frame)
  (require 'lsp-ui-doc))

(defun lsp-ui--workspace-path (path)
  "Return the path relative to the workspace.
If the path is not in the workspace, it returns the original PATH."
  (let* ((root (lsp--workspace-root lsp--cur-workspace))
         (in-workspace (string-prefix-p root path)))
    (if in-workspace
        (substring path (length root))
      path)))

(defun lsp-ui--toggle (enable)
  "ENABLE."
  (dolist (feature '(lsp-ui-flycheck lsp-ui-peek lsp-ui-sideline lsp-ui-doc))
    (when (featurep feature)
      (let* ((sym (intern-soft (concat (symbol-name feature) "-enable")))
             (value (symbol-value sym))
             (fn (symbol-function sym)))
        (when (and (or value (not enable))
                   (functionp fn))
          (funcall fn enable))))))

(defvar lsp-ui-mode-map (make-sparse-keymap))

(define-minor-mode lsp-ui-mode
  "Toggle language server UI mode on or off.
‘lsp-ui-mode’ is a minor mode that contains a series of useful UI
integrations for ‘lsp-mode’.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is ‘toggle’."
  :init-value nil
  :group lsp-ui
  (lsp-ui--toggle lsp-ui-mode))

;; The request is delegated to xref-backend-apropos defined in lsp-mode.
;; xref-find-apropos does similar job but is less appealing because it splits and
;; regex quotes the pattern. The language server likely knows more about how
;; to do fuzzy matching.
(defun lsp-ui-find-workspace-symbol (pattern)
  "List project-wide symbols matching the query string"
  (interactive (list (read-string
                      "workspace/symbol: "
                      nil 'xref--read-pattern-history)))
  (xref--find-xrefs pattern 'apropos pattern nil))

(provide 'lsp-ui)
;;; lsp-ui.el ends here
