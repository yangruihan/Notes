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

### 依赖
`markdown-mode`依赖于`cl-lib`，`cl-lib`自GNU Emacs 24.3之后才默认自带。GNU Emacs 24.1和2。4.2 的用户可以通过`package.el`来安装`cl-lib`。

## 使用
快捷键绑定根据其功能按前缀进行分组。举个例子，用来插入一个链接的命令使`C-c C-a`，其中`C-a`是对HTML`<a>`标签的一种表示。其他情况下，快捷键前缀与HTML的联系不是直接的。举个例子，用来处理标题的命令以`C-c C-t`（`C-t`代表titling）开头。每一组的主要命令将在下面描述。你可以通过快捷键`C-c C-h`来获得快捷键绑定列表。移动和位移指令往往与配对的分隔符有关，如`M-{`和`M-}`或`C-c <`和`C-c >`。大纲导航的快捷键与`org-mode`使一致的。最后，用于运行Markdown或对打开的文件进行维护的命令分组在`C-c C-c`前缀下。你可以通过快捷键`C-c C-h`来获得快捷键绑定列表。

- 超链接：`C-c C-a`

    |快捷键|作用|
    |:---:|:---:|
    |C-c C-a l|插入`[text](url)`|
    |C-c C-a L|插入`[text][label]`|
    |C-c C-a u|插入`<url>`|
    |C-c C-a f|在该点插入一个脚标注即|
    |C-c C-a w|插入`[[]]`|

- 图片：`C-c C-i`

    |快捷键|作用|
    |:---:|:---:|
    |C-c C-i i|插入一个内联图片`![]()`|
    
- 样式：`C-c C-s`

    |快捷键|作用|
    |:---:|:---:|
    |C-c C-s e|使一个区域字变成斜体`*text*`|
    |C-c C-s b|添加引用`> text`|
    |C-c C-s C-b|使一个区域变成引用|
    |C-c C-s p|添加代码块|

- 标题：`C-c C-t`

    |快捷键|作用|
    |:---:|:---:|
    |C-c C-t h|自动根据层级插入标题`#`|
    |C-c C-k|删除标题|
    
- 水平标尺：`C-c -`

    |快捷键|作用|
    |:---:|:---:|
    |C-c -|插入`------------`|
    
- Markdown和维护命令：`C-c C-c`

    |快捷键|作用|
    |:---:|:---:|
    |C-c C-c m|编译|
    |C-c C-c p|预览|
    |C-c C-c e|将结果保存到`basename.html`|
    |C-c C-c v|导出文件并且在浏览器中预览|
    |C-c C-c o|打开Markdown源文件|
    |C-c C-c l|实时预览|
    |C-c C-c c|检查未定义的引用|
    |C-c C-c n|将缓冲区的列表重新排序|
    |C-c C-c ]|在缓冲中完成所有标题和标准化的所有水平的规则|

    总结一下：
    - C-c C-c m: markdown-command > *markdown-output* buffer.
    - C-c C-c p: markdown-command > temporary file > browser.
    - C-c C-c e: markdown-command > basename.html.
    - C-c C-c v: markdown-command > basename.html > browser.
    - C-c C-c w: markdown-command > kill ring.
    - C-c C-c o: markdown-open-command.
    - C-c C-c l: markdown-live-preview-mode > *eww* buffer.

TODO ...
