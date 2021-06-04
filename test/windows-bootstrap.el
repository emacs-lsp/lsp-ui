;;; windows-bootstrap.el --- Windows test bootstrap -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 emacs-lsp maintainers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Windows test bootstrap
;;
;;; Code:

(require 'package)

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
       (pkgs (append '(dash lsp-mode markdown-mode)
                     '(ert-runner flycheck rustic))))
  (package-initialize)
  (package-refresh-contents)

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-refresh-contents) (package-install pkg)))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda ()
               (unless (boundp 'emacs-lsp-ci)
                 (delete-directory ,user-emacs-directory t)))))

;;; windows-bootstrap.el ends here
