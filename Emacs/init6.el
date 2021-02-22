(setq gc-cons-threshold 50000000)

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "~/.emacs-backups"))))


;; ----------------------------
(defvar myPackages
  '(
    ;; common
    better-defaults
    flycheck
    multiple-cursors
    neotree
    rtags
    auto-complete
    all-the-icons
    transpose-frame

    ;; theme
    monokai-theme
    material-theme
    spacemacs-theme

    ;; python
    elpy
    py-autopep8

    ;; c/c++
    irony
    company-irony
    auto-complete-clang
    ;;cmake-ide
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
;; ----------------------------


(elpy-enable)
(require 'all-the-icons)
(require 'transpose-frame)


;; ----------------------------
;; flycheck config
;; ----------------------------
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (global-flycheck-mode)
;; ----------------------------


;; ----------------------------
;; py-autopep8 config
;; ----------------------------
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; ----------------------------


;; ----------------------------
;; neotree config
;; ----------------------------
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'icons)
(setq neo-window-fixed-size nil)
;; ----------------------------


;; ----------------------------
;; rtags config
;; ----------------------------
(require 'rtags)
(setq rtags-path "/usr/local/bin")
(setq rtags-rc-binary-name (or rtags-rc-binary-name "rc")
      rtags-rdm-binary-name (or rtags-rdm-binary-name "rdm"))
;; ----------------------------


;; ----------------------------
;; cmake-ide config
;; ----------------------------
;;(cmake-ide-setup)
;; ----------------------------


;; ----------------------------
;; irony config
;; ----------------------------
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq company-irony-ignore-case t)
;; ----------------------------


;; ----------------------------
;; auto-complete-clang config
;; ----------------------------
;;(add-to-list 'load-path (concat myoptdir "AC"))
(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories (concat myoptdir "AC/ac-dict"))
(require 'auto-complete-clang)
(ac-config-default)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(add-hook 'after-init-hook 'global-company-mode)
;; ----------------------------


;; ----------------------------
;; multiple-cursors
;; ----------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; ----------------------------


;; ----------------------------
;; 基本设置
;; ----------------------------
(setq inhibit-startup-message t)
(load-theme 'spacemacs-dark t)
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
;; ----------------------------


;; ----------------------------
;; 自定义函数
;; ----------------------------

;; ----------------------------
;; 设置重新载入 buffer 而不需要确认
;; ----------------------------
(defun revert-buffer-no-confirm()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
;; ----------------------------


;; ----------------------------
;; 设置 org 模式下，插入不同风格的代码段
;; ----------------------------
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
;; ----------------------------


;; ----------------------------
;; 设置 F7 切换透明和非透明
;; ----------------------------
(global-set-key [(f7)] 'loop-alpha)

(setq alpha-list '((35 15) (65 30) (85 55) (100 100)))

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
;; ----------------------------


;; ----------------------------
;; 自定义参数
;; ----------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages
   (quote
    (monokai-theme py-autopep8 neotree material-theme flycheck elpy better-defaults)))
  '(linum-format 'dynamic)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ----------------------------


;; ----------------------------
;; set tab width for text-mode
(add-hook 'text-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq indent-line-function (quote insert-tab))))
;; ----------------------------


;; -------------------------------------
;; Shift + Tab 左移 4 个空格
;; set shift tab remove 4 spaces
;; -------------------------------------
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of the line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match ""))
      (when (looking-at "^\t")
        (replace-match "")))))
;; ----------------------------
