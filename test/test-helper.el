;;; -*- lexical-binding: t; -*-


(add-to-list 'load-path
             (file-name-as-directory (f-parent (f-parent (f-this-file)))))

(require 'lsp-mode)
(require 'lsp-rust)
(require 'lsp-ui)
(require 'flycheck)
(require 'lsp-ui-flycheck)
(require 'rustic)

;; don't start LSP server for every test
(setq rustic-lsp-setup-p nil)

(setq lsp-restart 'ignore)

(defun lsp-ui-test-generate-project ()
  (let* ((default-directory "/tmp")
         (dir (make-temp-file-internal "cargo" 0 "" nil)))
    (shell-command-to-string (format "cargo new %s --bin --quiet" dir))
        (concat (expand-file-name dir) "/")))

(defun lsp-ui-test-create-project-buffer (buffer string)
  "Populate BUFFER with STRING."
  (let* ((dir (lsp-ui-test-generate-project))
         (src (concat dir "/src"))
         (file (expand-file-name "main.rs" src))
         (rustic-format-trigger nil))
    (with-current-buffer buffer
      (write-file file)
      (insert string)
      (save-buffer))
    dir))


;;; test-helper.el ends here
