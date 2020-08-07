```emacs lisp
(find-file "/ssh:parallels@10.211.55.7:/")

(defun centos-shell ()
  (interactive)
  (let ((default-directory "/ssh:parallels@10.211.55.7:/")
	    (explicit-shell-file-name "/bin/bash"))
    (shell (generate-new-buffer-name "*centos*"))))

(defun bandwagon-shell ()
  (interactive)
  (let ((default-directory "/sshx:root@10.22.17.13#29:/root/"))
    (shell (generate-new-buffer-name "*bandwagon*"))))
```

编辑~/.ssh/config 加入如下内容

Example for sshx

```
Host bandwagon
     HostName 10.22.17.13
     Port 29
     User root
```

Example for ssh

```
Host centos
     HostName 10.211.55.4
     User parallels
```

上面的代码就可以缩短成"/sshx:bandwagon:/root/"

```emacs lisp
;; Make shell & ssh
(defmacro make--shell (name &rest arglist)
  `(defun ,(intern (format "%S-shell" name)) ,arglist
     (interactive)
     (let ((default-directory ,(format "/ssh:%S:" name))
           (explicit-shell-file-name "/bin/bash"))
       (shell (generate-new-buffer-name ,(format "*%S*" name))))))

(defmacro make--ssh (name &rest arglist)
  `(defun ,(intern (format "%S-ssh" name)) ,arglist
     (interactive)
     (find-file ,(format "/ssh:%S:" name))))

(make--ssh centos)
(make--shell centos)
```

```
(make--ssh "storage" "admin@xx.xx.xx.xx")
(make--shell "storage" "admin@xx.xx.xx.xx")
(make--ssh "gpu" "root@xx.xx.xx.xx")
(make--shell "gpu" "root@xx.xx.xx.xx")
```
