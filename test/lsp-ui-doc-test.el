;; -*- lexical-binding: t -*-


(require 'lsp-ui-doc)

(ert-deftest lsp-ui-doc--make-smaller-empty-lines ()
  "Test if `lsp-ui-doc--make-smaller-empty-lines' correctly replaces lines"
  (let ((string "This is a test

It will show it will shrink empty lines
"))
    (with-temp-buffer
      (insert string)
      (lsp-ui-doc--make-smaller-empty-lines)
      (should (ert-equal-including-properties
               #("
This is a test
 
It will show it will shrink empty lines


"
                 0 1 (lsp-ui-doc-no-space t face (:height 0.2))
                 16 17 (lsp-ui-doc-no-space t face (:height 0.5))
                 17 18 (face (:height 0.5))
                 58 60 (lsp-ui-doc-no-space t face (:height 0.3)))
               (buffer-substring (point-min) (point-max)))))))


(ert-deftest lsp-ui-doc--handle-hr-lines ()
  "Test if `lsp-ui-doc--handle-hr-lines' correctly replaces markdown hrules"
  (let ((string "Before

---
Text

---
After"))
    (with-temp-buffer
      (insert string)
      (markdown-mode)
      (lsp-ui-doc--handle-hr-lines)
      (should (ert-equal-including-properties
               #("Before

   
Text

   
After"
                 8 9 (display (space :height (1)) lsp-ui-doc--replace-hr t face (:background "dark grey"))
                 9 10 (display (space :height (1)))
                 10 12 (face (:height 0.2))
                 18 19 (display (space :height (1)) lsp-ui-doc--replace-hr t face (:background "dark grey"))
                 19 20 (display (space :height (1)))
                 20 22 (face (:height 0.2)))
               (buffer-substring (point-min) (point-max)))))))
