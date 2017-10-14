# Mac Reset

## 1. Setting

1. 键盘设置中文

​	键盘 -> 输入源 -> ➕ -> 简体中文

2. 触控板设置

​	系统偏好设置 -> 触控板

​	- 光标与点按页签 -> 轻点来点按

​	- 滚动缩放 -> **取消** 滚动方向：自然

​	辅助功能 -> 鼠标与触控板 -> 触控板选项 -> 启动拖移 -> 三指拖移

3. 关闭App Store 自动更新

​	系统偏好设置 -> App Store -> **取消** 自动检查更新

4. 防止系统进入睡眠

​	系统偏好设置 -> 节能 -> **勾选** 当显示器关闭时，防止电脑自动进入睡眠

5. 开启 F1、F2等功能键

​	系统偏好设置 -> 键盘 -> **勾选** 将F1、F2等键用作标准功能

## 2. Apps

| 名称             | 作用      |
| :------------- | :------ |
| Shadowsocket   | 搭梯子     |
| 搜狗输入法          | 输入法     |
| Chrome         | 浏览器     |
| Moom           | 调节窗口大小  |
| AppCleaner     | 卸载App工具 |
| The Unarchiver | 压缩软件    |
| Manico         | 快速切换应用  |
| Sublime Text3  | 编辑器     |
| Alfred 3       | 启动器     |
| iTerm          | 加强终端    |
| iStat menu     | 系统监控    |



## 3. Command Line Tools

1. HomeBrew

   `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

2. On-My-Zsh

   `sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"`

3. Pyenv && Pyenv-virtualenv

   `brew update`

   `brew install pyenv`

   `brew install pyenv-virtualenv`

   ```bash
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
   ```

   如果在安装版本中速度太慢，可以使用[国内镜像加速](http://www.jianshu.com/p/cb7a128b284b)。

   如果在安装版本中出现以下错误：

   ```
   ERROR: The Python ssl extension was not compiled. Missing the OpenSSL lib?
   ```

   可以通过执行以下命令安装，参考地址 https://github.com/pyenv/pyenv/issues/950：

   ```
   brew uninstall openssl && brew install openssl && CFLAGS="-I$(brew --prefix openssl)/include" LDFLAGS="-L$(brew --prefix openssl)/lib" pyenv install 3.6.2
   ```

4. Git

   `brew install git`

   Git设置及GitHub配置参考[这里](https://github.com/yangruihan/Notes/blob/master/Git/Git%20SSH%20Key%20生成并添加到%20Github%20步骤.md)。

