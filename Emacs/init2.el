;; 插件设置
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(add-hook 'after-init-hook 'global-company-mode)

;; 基本设置
;; --------------------------------------

(setq inhibit-startup-message t) ;隐藏开启信息
(load-theme 'material t) ;加载material主体
(global-linum-mode t) ;显示行号
(setq column-number-mode t) ;显示列号

;; 括号匹配时显示另一个括号而不是跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(setq frame-title-format "%b %I") ;显示文件名和大小
(auto-image-file-mode t) ;让Emacs可以直接打开、显示图片
(fset 'yes-or-no-p 'y-or-n-p) ;以Y/N代表yes/no
(setq auto-save-default nil) ;不生成名为#filename#的临时文件
(setq x-select-enable-clipboard t) ;支持和外部程序的拷贝
(global-font-lock-mode t) ;打开语法高亮
; (set-default-font "Monaco-14") ;设置默认字体

;; 设置空格代替tab键，并且宽度为4个空格
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "Linux")
(setq c-basic-offset 4)

;; 设置Org模式自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; 开启org-mode转markdown
(setq org-export-backends (quote (ascii html icalendar latex md)))

;; 打开文件时不打开新的窗口，而是作为当前窗口的一个Buffer
(setq ns-pop-up-frames nil)

;; 自定义函数
;; -----------------------------------------------------

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))
