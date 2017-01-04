# Emacs markdown-mode 说明
## 简介
markdown-mode 是一个用来编辑Markdown格式文件的主要模式。

## 安装
安装markdown-mode的推荐方式是使用`package.el`从[MELPA Stable](https://stable.melpa.org/#/markdown-mode)安装软件包。首先，通过将下面的内容添加到你的`.emacs`，`init.el`或其他等效的启动文件中来配置`package.el`和**MELPA Stable仓库**：
```[lisp]
(require 'package)
(add-to-list 'package-archives
             '("mepla-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

然后，在重启Emacs或手动使上述内容生效后，使用接下来的命令：`M-x package-install RET markdown-mode RET`。当使用这种方式进行安装时，主要的模式——`markdown-mode`和`gfm-mode`将被自动加载，并且当你编辑以`.md`或`.markdown`结尾文件名的文件时，会自动使用`markdown-mode`。

或者，如果你使用`use-package`来管理加载包时，你可以通过向初始化文件中添加一个声明来自动安装和配置`markdown-mode`（一个例子，你可以根据需要来调整设置）：
```[lisp]
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown:))
```

### 直接下载

你还可以手动下载和安装markdown-mode。首先，下载最新的稳定版本并且将它保存在Emacs可以找到的地方（比如，在你`load-path`中的一个路径）。然后，你可以通过向你的初始化文件中添加下面的内容来将`markdown-mode`和`gfm-mode`配置成自动加载：
```[lisp]
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "gfm-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
```

### 开发版本

为了跟随或促进markdown-mode的开发，你在Github上浏览和克隆该项目：
```[shell]
git clone https://github.com/jrblevin/markdown-mode.git
```

如果你想要安装和使用一个开发版本——有时候它是不稳定的，你也可以通过像上面那样克隆Git仓库或者从[MELPA](https://melpa.org/#/markdown-mode)安装。

如果你直接克隆仓库，那么请保证通过添加下面一行命令到你的启动文件中后Emacs可以找到它：
```[lisp]
(add-to-list 'load-path "/path/to/markdown-mode/repository")
```

### 包安装
markdown-mode 同时也存在与一些包管理系统中，你首先可能需要确认安装的软件包中是否包含最新的稳定版本（如果没有，请通知软件包维护者）。

- Debian Linux: [elpa-markdown-mode](https://packages.debian.org/sid/lisp/elpa-markdown-mode) and [emacs-goodies-el](http://packages.debian.org/emacs-goodies-el)
- Ubuntu Linux: [elpa-markdown-mode](http://packages.ubuntu.com/search?keywords=elpa-markdown-mode) and [emacs-goodies-el](http://packages.ubuntu.com/search?keywords=emacs-goodies-el)
- RedHat and Fedora Linux: [emacs-goodies](https://apps.fedoraproject.org/packages/emacs-goodies)
- NetBSD: [textproc/markdown-mode](http://pkgsrc.se/textproc/markdown-mode)
- MacPorts: [markdown-mode.el](https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile) ([pending](http://trac.macports.org/ticket/35716))
- FreeBSD: [textproc/markdown-mode.el](http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el)

TODO...
