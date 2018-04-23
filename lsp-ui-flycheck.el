;;; lsp-ui-flycheck.el --- Flycheck support for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017  fmdkdd
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui

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

;; Flycheck integration for lsp-mode.  To enable, put this in your config:
;; (require 'lsp-ui-flycheck)
;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

;;; Code:

(require 'lsp-notifications)
(require 'lsp-mode)
(require 'flycheck)
(require 'pcase)
(require 'dash)

(defgroup lsp-ui-flycheck nil
  "The LSP extension to display syntax checking."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-flycheck) Top")
  :link '(info-link "(lsp-ui-flycheck) Customizing"))

(defcustom lsp-ui-flycheck-enable t
  "Whether or not to enable ‘lsp-ui-flycheck’."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-flycheck-live-reporting t
  "If non-nil, diagnostics in buffer will be reported as soon as possible.
Typically, on every keystroke.
If nil, diagnostics will be reported according to `flycheck-check-syntax-automatically'."
  :type 'boolean
  :group 'lsp-ui-flycheck)

(defcustom lsp-ui-flycheck-list-position 'bottom
  "Position where `lsp-ui-flycheck-list' will show diagnostics for the whole workspace."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right))
  :group 'lsp-ui-flycheck)

(defvar-local lsp-ui-flycheck-list--buffer nil)

(defun lsp-ui-flycheck-list--post-command ()
  (when (eobp)
    (forward-line -1)))

(defun lsp-ui-flycheck-list--update (window workspace)
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
                 (let* ((message (or (lsp-diagnostic-message diag) "???"))
                        (severity (or (lsp-diagnostic-severity diag) 1))
                        (line (or (lsp-diagnostic-line diag) 1))
                        (face (cond ((= severity 1) 'error)
                                    ((= severity 2) 'warning)
                                    (t 'success)))
                        (text (concat (propertize (number-to-string line) 'face face)
                                      ": "
                                      (car (split-string message "\n")))))
                   (add-text-properties 0 (length text) `(diag ,diag file ,file window ,window) text)
                   (insert (concat text "\n")))))
             lsp--diagnostics))
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
    (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck-list--refresh nil t)
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
               (file (get-text-property (point) 'file))
               (window (get-text-property (point) 'window))
               (line (lsp-diagnostic-line diag))
               (column (lsp-diagnostic-column diag))
               (marker (with-current-buffer
                           (or (get-file-buffer file)
                               (find-file-noselect file))
                         (save-restriction
                           (widen)
                           (save-excursion
                             (goto-char 1)
                             (forward-line line)
                             (forward-char column)
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

(defvar lsp-ui-flycheck-list-mode-map nil
  "Keymap for ‘lsp-ui-flycheck-list-mode’.")
(unless lsp-ui-flycheck-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'lsp-ui-flycheck-list--quit)
    (define-key map (kbd "<return>") 'lsp-ui-flycheck-list--view)
    (define-key map (kbd "<M-return>") 'lsp-ui-flycheck-list--visit)
    (setq lsp-ui-flycheck-list-mode-map map)))

(define-derived-mode lsp-ui-flycheck-list-mode special-mode "lsp-ui-flycheck-list"
  "Mode showing flycheck diagnostics for the whole workspace."
  (setq truncate-lines t)
  (setq mode-line-format nil)
  (add-hook 'post-command-hook 'lsp-ui-flycheck-list--post-command nil t))

(defun lsp-ui-flycheck--start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  ;; Turn all errors from lsp--diagnostics for the current buffer into
  ;; flycheck-error objects and pass them immediately to the callback
  (let ((errors))
    (dolist (diag (or (gethash buffer-file-name lsp--diagnostics)
                      (gethash (file-truename buffer-file-name) lsp--diagnostics)))
      (push (flycheck-error-new
             :buffer (current-buffer)
             :checker checker
             :filename buffer-file-name
             :line (1+ (lsp-diagnostic-line diag))
             :column (1+ (lsp-diagnostic-column diag))
             :message (lsp-diagnostic-message diag)
             :level (pcase (lsp-diagnostic-severity diag)
                      (1 'error)
                      (2 'warning)
                      (_ 'info))
             :id (lsp-diagnostic-code diag))
            errors))
    (funcall callback 'finished errors)))

(flycheck-define-generic-checker 'lsp-ui
  "A syntax checker using the Language Server Protocol (RLS)
provided by lsp-mode.

See https://github.com/emacs-lsp/lsp-mode."
  :start #'lsp-ui-flycheck--start
  :modes '(python-mode) ; Need a default mode
  :predicate (lambda () lsp-mode)
  :error-explainer (lambda (e) (flycheck-error-message e)))

(defun lsp-ui-flycheck-add-mode (mode)
  "Add MODE as a valid major mode for the lsp checker."
  (unless (flycheck-checker-supports-major-mode-p 'lsp-ui mode)
    (flycheck-add-mode 'lsp-ui mode)))

(defun lsp-ui-flycheck--report nil
  (and flycheck-mode
       lsp-ui-flycheck-live-reporting
       (flycheck-buffer)))

;; FIXME: Provide a way to disable lsp-ui-flycheck
(defun lsp-ui-flycheck-enable (_)
  "Enable flycheck integration for the current buffer."
  (when lsp-ui-flycheck-live-reporting
    (setq-local flycheck-check-syntax-automatically nil))
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui)
  (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck--report nil t))

;; lsp-ui.el loads lsp-ui-flycheck.el, so we can’t ‘require’ lsp-ui.
;; FIXME: Remove this cyclic dependency.
(declare-function lsp-ui--workspace-path "lsp-ui" (path))

(provide 'lsp-ui-flycheck)
;;; lsp-ui-flycheck.el ends here
