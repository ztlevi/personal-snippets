# Deprecated due to lsp already implemented this feature

```emacs-lisp
;; add the following function to js2-mode-hook
(setq imenu-create-index-function 'js2-imenu-make-index)


;; this imenu generic expression aims to exclude for, while, if when aims to match functions in
;; es6 js, e.g. ComponentDidMount(), render() function in React
;; https://emacs-china.org/t/topic/4538/7
(defun js-exception-imenu-generic-expression-regexp ()
  ;; (async)? xxx (e) { }
  (if (re-search-backward "^[ \t]*\\(async\\)?[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" nil t)
      (progn
        (if (member (match-string 2) '("for" "if" "while" "switch" "function"))
            (js-exception-imenu-generic-expression-regexp)
          t))
    nil))

(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(
                               ;; =========================== AngularJS ========================
                               ("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                               ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                               ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                               ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *" 1)
                               ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *" 1)
                               ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                               ;; =========================== AngularJS ========================

                               ;; ============================== ES6,7 ===========================
                               ("Class" "^[ \t]*[0-9a-zA-Z_$ ]*[ \t]*class[ \t]*\\([a-zA-Z_$.]*\\)" 1)
                               ("Class" "^[ \t]*\\(var\\|let\\|const\\)[ \t]*\\([0-9a-zA-Z_$.]+\\)[ \t]*=[ \t]*[a-zA-Z_$.]*.extend" 2)
                               ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\..+\.extend" 1)

                               ;; async function xxx (
                               ("Function" "[ \t]*async[ \t]*function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ;; function(*)? xxx (
                               ("Function" "[ \t]*function\\(*\\)?[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 2)
                               ;; xxx : async function (
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*async[ \t]*function[ \t]*(" 1)
                               ;; xxx : function(*)? (
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function\\(*\\)?[ \t]*(" 1)
                               ;; (export)? (var|let|const)? xxx = async function (
                               ("Function" "^[ \t]*\\(export\\)?[ \t]*\\(var\\|let\\|const\\)?[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*async[ \t]*function[ \t]*(" 3)
                               ;; (export)? (var|let|const)? xxx = function* (
                               ("Function" "^[ \t]*\\(export\\)?[ \t]*\\(var\\|let\\|const\\)?[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function\\(*\\)?[ \t]*(" 3)

                               ;; (async)? xxx (e) { }
                               ("Function" js-exception-imenu-generic-expression-regexp 2)
                               ;; xxx : (async)? (
                               ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$.]*\\)[ \t]*:[ \t]*\\(async\\)?[ \t]*(" 1)
                               ;; (export)? (var|let|const)? xxx = (async)? (
                               ("Function" "^[ \t]*\\(export\\)?[ \t]*\\(var\\|let\\|const\\)?[ \t]*\\([A-Za-z_$][A-Za-z0-9_$.]*\\)[ \t]*=[ \t]*\\(async\\)?[ \t]*(" 3)
                               ;; (export)? (var|let|const)? xxx = (async)? e =>
                               ("Function" "^[ \t]*\\(export\\)?[ \t]*\\(var\\|let\\|const\\)?[ \t]*\\([A-Za-z_$][A-Za-z0-9_$.]*\\)[ \t]*=[ \t]*\\(async\\)?[ \t]*[A-Za-z_$][A-Za-z0-9_$.]*[ \t]*=>" 3)
                               ;; ============================== ES6,7 ===========================

                               ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
```
