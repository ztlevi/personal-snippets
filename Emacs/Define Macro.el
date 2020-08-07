;; Example 1
(defmacro ztlevi|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "ztlevi/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))

;; Example 2 使用宏可以减少重复的代码, 以下是一个使用宏来定义函数的例子:
(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
	   (url-hexify-string
	    (if mark-active
		(buffer-substring (region-beginning) (region-end))
	      (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)                   ; #1
  "Given some information regarding a search engine, install the interactive command to search through them"    
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()                                                       ; #2
       ,(format "Search %s with a query or region if any." search-engine-name)                                        ; #3
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))                                                    ; #4

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")                ; #5
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

第#1 行, 通过 prelude-install-search-engine 定义了一个需要 3 个参数的宏, 这个 宏的作用是生成一个函数.

第#2 行, 通过 intern 生成一个符号作为函数名, 名称为　*prelude-xxx* , 其中 xxx 为第一个参数的值.

第#3 行, 生成了这个函数的描述.

第#4 行, 调用 prelude-search 函数进行搜索处理.

第#5 行, 调用这个宏定义了一个名为　*prelude-google* 的函数.

从以上代码可以知道, 我们利用宏生成了４个名称不同的函数, 避免了手动编写函数的问题 (因为这４个函数的代码非常相似, 根据 DRY 原则应该尽量避免做这种重复工作).

