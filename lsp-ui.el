;;; lsp-ui.el --- UI modules for lsp-mode            -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Author:  Tobias Pisani <topisani@hamsterpoison.com>
;; Keywords: lsp
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Package-Requires: ((emacs "25.1") (flycheck "30") (lsp-mode "3.4"))
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

(require 'imenu)
(require 'lsp-mode)

(defun lsp-ui-imenu-create-index ()
  (mapcar
   (lambda (info)
     (let ((p (lsp--position-to-point
               (gethash "start" (gethash "range" (gethash "location" info))))))
       (cons (gethash "name" info)
             (if imenu-use-markers
                 (save-excursion (goto-char p) (point-marker))
               p))))
   (lsp--send-request
    (lsp--make-request "textDocument/documentSymbol"
                       (list :textDocument (lsp--text-document-identifier))))))

(provide 'lsp-ui)
;;; lsp-ui.el ends here
