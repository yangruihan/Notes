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
    monokai-theme
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
;; --------

(setq inhibit-startup-message t)
(load-theme 'monokai t)
(global-linum-mode t)
(setq linum-format "%d ")
(setq column-number-mode t)

(show-paren-mode t)
(setq show-paren-style 'parentheses)

(setq frame-title-format "%b %I")
(auto-image-file-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq x-select-enable-clipboard t)
(global-font-lock-mode t)

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "Linux")
(setq c-basic-offset 4)

;; 关闭自动补全自动变小写
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.1)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setq org-export-backends (quote (ascii html icalendar latex md)))

(setq ns-pop-up-frames nil)

;; 自定义函数
;; ----------

;; 设置重新载入 buffer 而不需要确认
;; ----------
(defun revert-buffer-no-confirm()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
;; ----------

;; 设置 org 模式下，插入不同风格的代码段
;; ----------
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
;; ----------

;; 设置 F7 切换透明和非透明
;; ----------
(global-set-key [(f7)] 'loop-alpha)

(setq alpha-list '((85 55) (100 100)))

(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)
;; ----------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme py-autopep8 neotree material-theme flycheck elpy better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set tab width for text-mode
(add-hook 'text-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq indent-line-function (quote insert-tab))))

