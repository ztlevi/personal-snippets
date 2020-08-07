(def-package! android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) #'+java|android-mode-maybe)
  :config
  (set! :yas-minor-mode 'android-mode)
  (set! :company-dict-minor-mode 'android-mode))

  ;;;###autoload
(defun +java|android-mode-maybe ()
  (when (doom-project-has! (or "local.properties"
                               "AndroidManifest.xml"
                               "src/main/AndroidManifest.xml"))
    (android-mode +1)
    (doom/set-build-command "./gradlew %s" "build.gradle")))


(defun doom--resolve-path-forms (paths &optional root)
  (cond ((stringp paths)
         `(file-exists-p
           (expand-file-name
            ,paths ,(if (or (string-prefix-p "./" paths)
                            (string-prefix-p "../" paths))
                        'default-directory
                      (or root `(doom-project-root))))))
        ((listp paths)
         (cl-loop for i in paths
                  collect (doom--resolve-path-forms i root)))
        (t paths)))

(defmacro doom-project-has! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  (doom--resolve-path-forms files (doom-project-root)))

  (defun doom-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))
