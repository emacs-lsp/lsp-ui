;; -*- lexical-binding: t -*-

(require 'lsp-modeline)

(ert-deftest lsp-ui-test-sideline-overlays ()
  "Basic test if overlays are stored in `lsp-ui-sideline--ovs' and set in buffer after call to
`lsp-ui-sideline-toggle-symbols-info'"
  (let ((rustic-lsp-setup-p t)
        (rustic-lsp-server 'rust-analyzer)
        (lsp-ui-sideline-show-diagnostics t))
    (let* ((string "fn main() {
    let bar = 1;
}")
           (buf (get-buffer-create "test-buffer"))
           (dir (lsp-ui-test-create-project-buffer buf string))
           (file (concat dir "src/main.rs")))
      (sit-for 3)
      (set-frame-size (selected-frame) 200 200)
      (with-current-buffer buf
        (goto-char (point-min))
        (forward-line 1)
        (flycheck-buffer)
        (sit-for 2)
        (lsp-ui-sideline-toggle-symbols-info)
        (sit-for 2)
        ;; test if ovs in `lsp-ui-sideline--ovs' contain the correct message
        (let* ((ov1 (nth 0 lsp-ui-sideline--ovs))
               (ov2 (nth 1 lsp-ui-sideline--ovs))
               (msg1 " `#[warn(unused_variables)]` on by default [unused_variables]")
               (msg2 " unused variable: `bar`"))
          (should (string= (substring-no-properties (overlay-get ov1 'after-string)) msg1))
          (should (string= (substring-no-properties (overlay-get ov2 'after-string)) msg2))

          ;; get a list of overlays in current buffer and test if ovs from `lsp-ui-sideline--ovs'
          ;; are in this list
          (let ((buffer-overlays (overlays-in (point-min) (point-max))))
            (should (member ov1 buffer-overlays))
            (should (member ov2 buffer-overlays)))

          ;; check if position is stored in `lsp-ui-sideline--occupied-lines'
          (let ((ov1-start (overlay-start ov1))
                (ov2-start (overlay-start ov2)))
            (should (member ov1-start lsp-ui-sideline--occupied-lines))
            (should (member ov2-start lsp-ui-sideline--occupied-lines)))))
        (sit-for 1)
        (should-not (lsp-find-workspace 'rust-analyzer default-directory))
      (kill-buffer buf))))
