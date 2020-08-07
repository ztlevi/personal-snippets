;;; prodigy.el -*- lexical-binding: t; -*-
  (prodigy-define-service
    :name "Leetcode Solution Website"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6005")
    :cwd "~/Developer/Github/leetcode"
    :tags '(leetcode)
    ;; if don't want to browse instantly, delete the following line
    :init (lambda () (browse-url "http://localhost:6005"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hackathon backend"
    :env '(("REDISCLOUD_URL" "redis://rediscloud:MeQVSBSNp82uhej2QW42vQxV2TEcd5xq@redis-14678.c44.us-east-1-2.ec2.cloud.redislabs.com:14678"))
    :command "npm"
    :args '("run" "start")
    :cwd "~/Developer/Github/cryptocurrency_exchange_app/backend"
    :tags '(express)
    :init (lambda () (switch-to-buffer "*prodigy-hackathon-backend*"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)
