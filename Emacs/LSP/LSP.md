# LSP

## create lsp client and run lsp in docker container

```emacs-lisp
  ;; Add the clangd client for C++ mode.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("lsp.sh" "clangd"
                       "mkdir -p .clangd && clangd --compile-commands-dir=/code --background-index=true --clang-tidy"))
    :major-modes '(c++-mode)
    :server-id 'my-clangd))
```

## LSP re-map keys

```lisp
  Add remapping until doom has better async solution
  (define-key! lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
    [remap xref-find-references]  #'lsp-ui-peek-find-references
    ;; `set-lookup-handlers!' won't work for lsp-ui-peek commands, because they
    ;; don't switch buffers
    [remap +lookup/definition] #'lsp-ui-peek-find-definitions
    [remap +lookup/references] #'lsp-ui-peek-find-references)
```

## Silence lsp errors

LSP 服务器端的错误, 如果你不想每打一个字就报错, 可以试一下下面的代码:

```emacs-lisp
(setq lsp--silent-errors
      '(
        -32800                          ;default error in lsp-mode
        -32603                          ;error that type in {...}
        ))
```

方法就是, 用 `(setq lsp-print-io t)` 打开调试信息, 然后 lsp 报错的时候, 会把服务器返回的错误代码打印出来, 然后加到
lsp–-silent-errors 列表中, 就可以在给 lsp 后端报 bug 的时候不被这些错误打扰.
