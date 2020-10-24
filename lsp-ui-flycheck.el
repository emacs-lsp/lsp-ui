;;; lsp-ui-flycheck.el --- Flycheck support for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017  fmdkdd
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languagues, tools
;; Version: 6.2

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck integration for lsp-mode.

;;; Code:

(require 'flycheck)
(require 'pcase)
(require 'dash)

(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-ui-flycheck nil
  "The LSP extension to display syntax checking."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-flycheck) Top")
  :link '(info-link "(lsp-ui-flycheck) Customizing"))

(defcustom lsp-ui-flycheck-list-position 'bottom
  "Position where `lsp-ui-flycheck-list' will show diagnostics for the whole workspace."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right))
  :group 'lsp-ui-flycheck)

(defvar-local lsp-ui-flycheck-list--buffer nil)
(defvar-local lsp-ui-flycheck--save-mode nil)

(defun lsp-ui-flycheck-list--post-command ()
  (when (eobp)
    (forward-line -1)))

(defun lsp-ui-flycheck-list--update (window workspace)
  "Update flycheck buffer in WINDOW belonging to WORKSPACE.
Use `lsp-diagnostics' to receive diagnostics from your LSP server."
  (let ((buffer-read-only nil)
        (lsp--cur-workspace workspace))
    (erase-buffer)
    (remove-overlays)
    (maphash (lambda (file diagnostic)
               (when diagnostic
                 (overlay-put
                  (make-overlay (point) (point))
                  'after-string
                  (concat (propertize "\n" 'face '(:height 0.2))
                          (propertize (lsp-ui--workspace-path file)
                                      'face 'dired-directory)
                          (propertize "\n" 'face '(:height 0.2)))))
               (dolist (diag diagnostic)
                 (-let* (((&Diagnostic :message :severity? :source?
                                       :range (&Range :start (&Position :line start-line))) diag)
                         (formatted-message (or (if source? (format "%s: %s" source? message) message) "???"))
                         (severity (or severity? 1))
                         (line (1+ start-line))
                         (face (cond ((= severity 1) 'error)
                                     ((= severity 2) 'warning)
                                     (t 'success)))
                         (text (concat (propertize (number-to-string line) 'face face)
                                       ": "
                                       (car (split-string formatted-message "\n")))))
                   (add-text-properties 0 (length text) `(diag ,diag file ,file window ,window) text)
                   (insert (concat text "\n")))))
             (lsp-diagnostics)))
  (if (= (point) 1)
      (overlay-put (make-overlay 1 1)
                   'after-string "No diagnostic available\n")
    (goto-char 1))
  (lsp-ui-flycheck-list-mode))

(defun lsp-ui-flycheck-list ()
  "List all the diagnostics in the whole workspace."
  (interactive)
  (let ((buffer (get-buffer-create "*lsp-diagnostics*"))
        (workspace lsp--cur-workspace)
        (window (selected-window)))
    (with-current-buffer buffer
      (lsp-ui-flycheck-list--update window workspace))
    (add-hook 'lsp-diagnostics-updated-hook 'lsp-ui-flycheck-list--refresh nil t)
    (setq lsp-ui-flycheck-list--buffer buffer)
    (let ((win (display-buffer-in-side-window
                buffer `((side . ,lsp-ui-flycheck-list-position) (slot . 5) (window-width . 0.20)))))
      (set-window-dedicated-p win t)
      (select-window win)
      (fit-window-to-buffer nil nil 10))))

(defun lsp-ui-flycheck-list--refresh ()
  (let ((workspace lsp--cur-workspace)
        (current-window (selected-window)))
    (when (and (buffer-live-p lsp-ui-flycheck-list--buffer)
               (get-buffer-window lsp-ui-flycheck-list--buffer)
               workspace)
      (with-selected-window (get-buffer-window lsp-ui-flycheck-list--buffer)
        (lsp-ui-flycheck-list--update current-window workspace)
        (fit-window-to-buffer nil nil 10)))))

(defun lsp-ui-flycheck-list--open ()
  (-when-let* ((diag (get-text-property (point) 'diag))
               ((&Diagnostic :range (&Range :start (&Position :line start-line
                                                              :character start-column))) diag)
               (file (get-text-property (point) 'file))
               (window (get-text-property (point) 'window))
               (marker (with-current-buffer
                           (or (get-file-buffer file)
                               (find-file-noselect file))
                         (save-restriction
                           (widen)
                           (save-excursion
                             (goto-char 1)
                             (forward-line start-line)
                             (forward-char start-column)
                             (point-marker))))))
    (set-window-buffer window (marker-buffer marker) t)
    (with-selected-window window
      (goto-char marker)
      (recenter)
      (pulse-momentary-highlight-one-line (marker-position marker) 'next-error))
    window))

(defun lsp-ui-flycheck-list--view ()
  (interactive)
  (lsp-ui-flycheck-list--open))

(defun lsp-ui-flycheck-list--visit ()
  (interactive)
  (select-window (lsp-ui-flycheck-list--open)))

(defun lsp-ui-flycheck-list--quit ()
  (interactive)
  (kill-buffer))

(defvar lsp-ui-flycheck-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'lsp-ui-flycheck-list--quit)
    (define-key map (kbd "<return>") 'lsp-ui-flycheck-list--view)
    (define-key map (kbd "<M-return>") 'lsp-ui-flycheck-list--visit)
    map)
  "Keymap for ‘lsp-ui-flycheck-list-mode’.")

(define-derived-mode lsp-ui-flycheck-list-mode special-mode "lsp-ui-flycheck-list"
  "Mode showing flycheck diagnostics for the whole workspace."
  (setq truncate-lines t)
  (setq mode-line-format nil)
  (add-hook 'post-command-hook 'lsp-ui-flycheck-list--post-command nil t))

(declare-function lsp-ui--workspace-path "lsp-ui" (path))

(provide 'lsp-ui-flycheck)
;;; lsp-ui-flycheck.el ends here
