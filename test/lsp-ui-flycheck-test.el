;; -*- lexical-binding: t -*-

(require 'lsp-modeline)

(ert-deftest lsp-ui-test-flycheck-list--update ()
  "Test if `lsp-ui-flycheck-list--update' populates buffer *lsp-diagnostics*."
  (let ((rustic-lsp-setup-p t)
        (rustic-lsp-server 'rust-analyzer))
    (let* ((string "fn main() {\nlet bar = 1;\nbar = bar + 2;}")
           (buf (get-buffer-create "test-buffer"))
           (dir (lsp-ui-test-create-project-buffer buf string))
           (file (concat dir "src/main.rs"))
           (diagnostics "3: rustc: value assigned to `bar` is never read
3: rustc: cannot assign twice to immutable variable `bar`
"))
      (sit-for 3)
      (with-current-buffer buf
        (lsp-ui-flycheck-list--update (selected-window) (lsp-find-workspace 'rust-analyzer default-directory))
        (should (string= (buffer-substring-no-properties (point-min) (point-max)) diagnostics))
        (with-lsp-workspace (lsp-find-workspace 'rust-analyzer default-directory)
          (lsp--shutdown-workspace)))
      (kill-buffer buf))))

(ert-deftest lsp-ui-test-flycheck-list-errors ()
  "Test if `flycheck-list-errors' displays clippy message."
  (let ((rustic-lsp-setup-p t)
        (rustic-lsp-server 'rust-analyzer)
        (lsp-rust-analyzer-cargo-watch-command "clippy"))
    (let* ((string "fn main() {
    let vec = vec![1];
    if vec.len() <= 0 {
        println!(\"testing\");
    }
}")
           (buf (get-buffer-create "test-buffer"))
           (dir (lsp-ui-test-create-project-buffer buf string))
           (file (concat dir "src/main.rs"))
           (diagnostics "this comparison involving the minimum or maximum element for this type contains a case that is always true or always false"))
      (sit-for 3)
      (with-current-buffer buf
        (flycheck-buffer)
        (sit-for 1)
        (flycheck-list-errors)
        (sit-for 1)
        (with-current-buffer (get-buffer "*Flycheck errors*")
          (should (string-match diagnostics (buffer-substring-no-properties (point-min) (point-max)))))
        (with-lsp-workspace (lsp-find-workspace 'rust-analyzer default-directory)
          (lsp--shutdown-workspace)))
      (kill-buffer buf))))
