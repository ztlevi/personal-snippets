(dap-register-debug-template
 "PYTHON DOCKER"
 (list :type "python"
       :cwd nil
       :request "launch"
       :name "Docker python debug"
       :args '()
       :pathMappings (ht ("~/project" (projectile-project-root (buffer-file-name))))
       :sourceMaps t))


(dap-register-debug-template
 "Attach to node process in docker container"
 (list :type "node"
       :request "attach"
       :port 9229
       :localRoot "<my_hardcoded_workspace_folder>"
       :remoteRoot "/app"
       :program "__ignored"
       :name "Attach to node process in docker container"))
