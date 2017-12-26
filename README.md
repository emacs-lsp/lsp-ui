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

### lsp-ui-sideline:
Show informations of the symbols on the current line.  
It also show flycheck diagnostics and LSP code actions  
![lsp-line](images/lsp-line.gif)

### lsp-ui-peek:
Add [peek](https://code.visualstudio.com/docs/editor/editingevolved#_peek) feature  
![lsp-xref](images/lsp-xref.gif)

### lsp-ui-doc:
Show object documentation at point in a child frame.  
![lsp-ui-doc](images/lsp-ui-doc.gif)
