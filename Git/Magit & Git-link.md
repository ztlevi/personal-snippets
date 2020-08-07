# Magit

1. Magit set upstream. Checkout PR uses upstream's remote. 比如我 clone 了一个 fork 到本地，那么这个时候即使你添加了
   upstream remote，但是仍然看不到上游的 PR。这是因为 magit 还是把 origin 作为默认的 upstream，而我的 fork 是没有 PR 的
   ，所以当然看不到任何 PR。解决办法是在本地 git config 文件中添加上这样两句：

   ```
   [magit]
      upstream = ztlevi
   ```

   其中第一个 upstream 是变量名（不能变），第二个 upstream 是我设置的 remote 名（可以根据你的设置调整）。然后再 checkout
   PR，就可以看到上游的 PR 了。

2. Git ignore locally

   Under `magit-status buffer`, use `C-u i l` to ignore files locally in _.git/info/exclude_.

3. Magit wip: It will make a commit every time we save a file. See the log tree with `magit-wip-log`, and can perform
   `reset` on the log tree.

   ```
   (magit-wip-after-save-mode)
   ```

4. Save popup args

   ```
   C-x C-s magit-popup-save-default-arguments
   ```

5. Auth token failed: Clean up your duplicate personal token from https://github.com/settings/tokens .

   https://magit.vc/manual/ghub/Manually-Creating-and-Storing-a-Token.html

# Git-link

Select remote: `C-u/SPC u` With a prefix argument prompt for the remote's name. Defaults to `"origin"`.

```
   (add-to-list 'git-link-remote-alist
               '("isl-122-ubuntu" git-link-gitlab))
```

## Customize remote URL

```emacs-lisp
(defun git-link-llvm (hostname dirname filename branch commit start end)
      (format "https://github.com/llvm-mirror/%s/tree/%s/%s"
              (file-name-base dirname)
              (or branch commit)
              (concat filename
                      (when start
                        (concat "#"
                                (if end
                                    (format "L%s-%s" start end)
                                  (format "L%s" start)))))))
  (defun git-link-musl (hostname dirname filename branch commit start end)
      (format "http://git.musl-libc.org/cgit/%s/tree/%s%s%s"
              (file-name-base dirname)
              filename
              (if branch "" (format "?id=%s" commit))
              (if start (concat "#" (format "n%s" start)) "")))
  (defun git-link-sourceware (hostname dirname filename branch commit start end)
    (format "https://sourceware.org/git/?p=%s.git;a=blob;hb=%s;f=%s"
            (file-name-base dirname)
            commit
            (concat filename
                    (when start
                      (concat "#" (format "l%s" start))))))
  (add-to-list 'git-link-remote-alist '("git.llvm.org" git-link-llvm))
  (add-to-list 'git-link-remote-alist '("git.musl-libc.org" git-link-musl))
  (add-to-list 'git-link-remote-alist '("sourceware.org" git-link-sourceware))
```
