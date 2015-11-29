# brew update 失败 Error: Error: Failure while executing: git pull -q origin refs/heads/master:refs/remotes/origin/master

```
$ cd /usr/local
$ git remote add origin git://github.com/mxcl/homebrew.git
fatal: remote origin already exists.
$ git fetch origin
$ sudo brew update
Already up-to-date
```

如果还不能解决，查看[参考资料](http://stackoverflow.com/questions/14113427/brew-update-failed)