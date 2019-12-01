# 如何在一台电脑使用多个github账号

1. 通过[
Git SSH Key 生成并添加到 Github 步骤](https://github.com/yangruihan/Notes/blob/master/Git/Git%20SSH%20Key%20生成并添加到%20Github%20步骤.md)，分别为两个账号生成对应的ssh key，注意**在生成的时候记得区分路径，避免后生成的key把前面的覆盖掉**

2. 在`~/.ssh`目录添加配置文件`config`，windows系统在`C:\Users\username\.ssh`，内容如下：

    ```
    #username1(username1@gmail.com)
    Host username1.github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile C:/Users/xxx/.ssh/id_rsa_username1
    User username1

    #username2(username2@qq.com)
    Host username2.github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile C:/Users/xxx/.ssh/id_rsa_username2
    User username2

    ```

    添加好后，可以在命令行进行测试

    ```
    ssh -T git@username1.github.com

    ssh -T git@username2.github.com
    ```

    如果出现类似下面的提示，则说明已经可以访问了：

    ```
    Hi xxx! You've successfully authenticated, but GitHub does not provide shell access.
    ```

3. 对应的代码库添加远程追踪时，需要将链接改成上面配置的 Host，比如，你想通过ssh访问代码库，地址是`git@github.com:username1/test.git`，在使用`git remote add`的时候需要将地址改为`git@username1.github.com:username1/test.git`，剩下的就可以正常使用了

4. 如何切换账号，可以在库目录下使用`git config --local user.name "xxx"`和`git config --local user.email "xxx"`来对不同的库进行不同的配置