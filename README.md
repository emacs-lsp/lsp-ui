# lsp-ui

This contains all the higher level UI modules of `lsp-mode`, like flycheck support and code lenses.

More info to follow.

To enable the package and its features:

``` el
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
```

To customize:  
`M-x customize-group [RET] lsp-ui [RET]`   
