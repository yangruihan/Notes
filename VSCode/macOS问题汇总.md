# Macos 出现更新报错 "Could not create temporary directory: 权限被拒绝" 如何解决？

解决方案
```sh
sudo chown $USER ~/Library/Caches/com.microsoft.VSCode.ShipIt/
```

然后重启，即可解决问题。
