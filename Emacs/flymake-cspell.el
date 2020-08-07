;;; ~/.doom.d/flymake-cspell.el -*- lexical-binding: t; -*-

(require 'flymake-easy)

(defgroup flymake-cspell nil
  "Flymake spell checking with cspell"
  :group 'programming
  :prefix "flymake-cspell-")

;; ;;;###autoload
;; (defcustom flymake-jslint-detect-trailing-comma t
;;   "Whether or not to report warnings about trailing commas."
;;   :type 'boolean :group 'flymake-jslint)

;;;###autoload
(defcustom flymake-cspell-command "cspell"
  "Name (and optionally full path) of jslint executable."
  :type 'string :group 'flymake-cspell)

;;;###autoload
(defcustom flymake-cspell-args '("--config" (eval (substitute-in-file-name "$HOME/.cspell.json")))
  "Command-line args for jslint executable."
  :type '(repeat string) :group 'flymake-cspell)

(defconst flymake-jslint-err-line-patterns
  '(("^ *#[0-9]+ \\(.*?\\)\n.*?// Line \\([0-9]+\\), Pos \\([0-9]+\\)$" nil 2 3 1)
    ;; jsl
    ("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$" nil 2 nil 4)))
(defconst flymake-jslint-trailing-comma-err-line-pattern
  '("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" nil 2 nil 3))

(defun flymake-jslint-command (filename)
  "Construct a command that flymake can use to check javascript source in FILENAME."
  (append
   (list flymake-jslint-command)
   flymake-jslint-args
   (unless (string-match "jslint" flymake-jslint-command)
     ;; jsl required option
     (list "-process"))
   (list filename)))

;;;###autoload
(defun flymake-jslint-load ()
  "Configure flymake mode to check the current buffer's javascript syntax."
  (interactive)
  (flymake-easy-load 'flymake-jslint-command
                     (append flymake-jslint-err-line-patterns
                             (when flymake-jslint-detect-trailing-comma
                               (list flymake-jslint-trailing-comma-err-line-pattern)))
                     'tempdir
                     "js"))


(provide 'flymake-cspell)
