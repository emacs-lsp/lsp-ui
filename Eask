(package "lsp-ui"
         "9.0.0"
         "UI modules for lsp-mode")

(website-url "https://github.com/emacs-lsp/lsp-ui")
(keywords "languages" "tools")

(package-file "lsp-ui.el")

(files "*.el" "lsp-ui-doc.html")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "dash")
(depends-on "lsp-mode")
(depends-on "markdown-mode")

(development
 (depends-on "ert-runner")
 (depends-on "rustic")
 (depends-on "flycheck"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
