# Git SSH Key 生成并添加到 Github 步骤
原文：[Git SSH Key 生成步骤](http://blog.csdn.net/hustpzb/article/details/8230454)

Git 是分布式的代码管理工具，远程的代码管理是基于SSH的，所以要使用远程的 Git 则需要SSH的配置。

**Github 的 SSH 配置步骤如下：**

## 一、设置Git user的name和email：
```
$ git config --global user.name "your user name"
$ git config --global user.email "your email address"
```

## 二、生成SSH密钥：
### 2.1 检查 ssh 密钥是否已存在
#### 2.1.1 Windows环境下
查看`C:\Users\YourUserName\.ssh`是否存在
如果该文件夹存在，则说明密钥已存在，则备份删除
#### 2.1.2 Linux环境下
查看`~/.ssh`是否存在
如果该文件夹存在，则说明密钥已存在，则备份删除

### 2.2 生成 ssh 密钥
在命令行中输入
```
$ ssh-keygen -t rsa -C “your email address”
```
连续按3个回车（密码默认为空），得到 id_rsa 和 id_rsa.pub 文件，说明生成成功

## 三、添加密钥到 Github
打开 [Github](http://www.github.com)，登录自己的账号后
点击自己的头像->settings->SSH Keys->Add SSH key
将本地 id_rsa.pub 中的内容粘贴到 Key 文本框中，随意输入一个 title，点击 Add Key 即可

## 四、测试
在命令行中输入
```
$ ssh git@github.com
```
会出现如下询问：
```
Are you sure you want to continue connecting (yes/no)?
```
键入`yes`后回车，如果出现
```
Hi xxx! You’ve successfully authenticated, but GitHub does not provide shell accessConnection to github.com closed.
```
则说明验证成功，否则可能是上述步骤中的其中几步出错了，需重新来过
