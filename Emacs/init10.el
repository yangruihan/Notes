(setq gc-cons-threshold 50000000)

(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.emacs-china.org/gnu/") t)
(add-to-list 'package-archives
             '("melpa" . "http://elpa.emacs-china.org/melpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://elpa.emacs-china.org/stable-melpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

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
    rg
    fzf
    ivy
    swiper
    counsel
    smex
    goto-chg
    origami
    crux
    w3m
    markdown-mode
    google-translate

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

    ;; rust
    rust-mode
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
;; ----------------------------


(elpy-enable)
(setq elpy-rpc-python-command "python3")

(require 'all-the-icons)
(require 'transpose-frame)


;; ----------------------------
;; emacs application framework
;; ----------------------------
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-org-previewer)
;; (require 'eaf-mindmap)
;; (require 'eaf-browser)
;; (require 'eaf-airshare)
;; (require 'eaf-image-viewer)
;; (require 'eaf-system-monitor)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-terminal)
;; (require 'eaf-file-sender)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-file-manager)
;; ----------------------------


;; ----------------------------
;; ivy config
;; ----------------------------
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

;; Ivy-resume and other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
; (global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)
;; ----------------------------

;; ----------------------------
;; origami config
;; ----------------------------
(add-hook 'prog-mode-hook
          (lambda () (origami-mode)))
(defvar origami-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'origami-open-all-nodes)
    (define-key map (kbd "<C-M-return>") #'origami-close-all-nodes)
    (define-key map (kbd "<C-return>") #'origami-open-node-recursively)
    map))
;; ----------------------------

;; ----------------------------
;; crux config
;; ----------------------------
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-S-RET") #'crux-smart-open-line-above)
(global-set-key (kbd "S-RET") #'crux-smart-open-line)
(global-set-key (kbd "C-c C-n") #'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c f") #'crux-recentf-find-file)
(global-set-key (kbd "C-c F") #'crux-recentf-find-directory)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
;; ----------------------------

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
;; markdown mode config 
;; ----------------------------
;; (use-package markdown-mode
;;              :ensure t
;;              :commands (markdown-mode gfm-mode)
;;              :mode (("README\\.md\\'" . gfm-mode)
;;                     ("\\.md\\'" . markdown-mode)
;;                     ("\\.markdown\\'" . markdown-mode))
;;              :init (setq markdown-command "multmarkdown"))
;; ;; ----------------------------


;; ----------------------------
;; Google translate config
;; ----------------------------
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)
;; ----------------------------


;; ----------------------------
;; rust config
;; ----------------------------
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
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
(global-set-key (kbd "C-M-x C-l") #'mc/edit-lines)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
;; ----------------------------


;; ----------------------------
;; 基本设置
;; ----------------------------

(load-theme 'spacemacs-dark t)

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "~/.emacs-backups"))))
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

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

;; 设置回到上一个修改处快捷键
(global-set-key (kbd "C-x C-[") #'goto-last-change)
(global-set-key (kbd "C-x C-]") #'goto-last-change-reverse)

;; ;; unicode characters in term-mode
;; (defadvice ansi-term (after advise-ansi-term-coding-system)
;;   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; (ad-activate 'ansi-term)
;; (setq ansi-color-for-comint-mode t)
;; 
;; 
;; ;; utf-8 support
;; (setq locale-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (modify-coding-system-alist 'process "*" 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8))
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-default buffer-file-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
;; (setq-default pathname-coding-system 'utf-8)
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; (defun change-shell-mode-coding ()
;;   (progn
;;     (set-terminal-coding-system 'utf-8)
;;     (set-keyboard-coding-system 'utf-8)
;;     (set-selection-coding-system 'utf-8)
;;     (set-buffer-file-coding-system 'utf-8)
;;     (set-file-name-coding-system 'utf-8)
;;     (modify-coding-system-alist 'process "*" 'utf-8)
;;     (set-buffer-process-coding-system 'utf-8 'utf-8)
;;     ))
;; 
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
(global-set-key (kbd "<S-tab>") #'un-indent-by-removing-4-spaces)
(global-set-key (kbd "<backtab>") #'un-indent-by-removing-4-spaces)
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

;; ----------------------------
;; Command alias
;; ----------------------------
(defalias 'rfsap 'rtags-find-symbol-at-point)
(defalias 'rfs   'rtags-find-symbol)
(defalias 'rfrap 'rtags-find-references-at-point)
(defalias 'rfr   'rtags-find-references)
;; ----------------------------

(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
