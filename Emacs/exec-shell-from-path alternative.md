## https://emacs-china.org/t/topic/5507

```shell
sh -c 'printf "%s" "$PATH"' > ~/.path
```

```emacs lisp
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (print path)
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err))))
```
