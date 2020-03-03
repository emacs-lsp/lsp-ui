;; -*- lexical-binding: t -*-

(ert-deftest lsp-ui-test-flycheck-list--update ()
  "Test if `lsp-ui-flycheck-list--update' populates buffer *lsp-diagnostics*."
  (let ((rustic-lsp-setup-p t)
        (rustic-lsp-server 'rust-analyzer))
    (setq lsp-log-io t)
    (let* ((string "fn main() {\nlet bar = 1;\nbar = bar + 2;}")
           (buf (get-buffer-create "test-buffer"))
           (dir (lsp-ui-test-create-project-buffer buf string))
           (file (concat dir "src/main.rs"))
           (diagnostics "2: rustc: value assigned to `bar` is never read
2: rustc: cannot assign twice to immutable variable `bar`
"))
      (sit-for 3)
      (with-current-buffer buf
        (lsp-ui-flycheck-list--update (selected-window) (lsp-find-workspace 'rust-analyzer default-directory))
        (should (string= (buffer-substring-no-properties (point-min) (point-max)) diagnostics))
        (print (buffer-name))
        (with-lsp-workspace (lsp-find-workspace 'rust-analyzer default-directory)
          (lsp--shutdown-workspace)))
      (kill-buffer buf))))

