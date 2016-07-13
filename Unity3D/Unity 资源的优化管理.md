# Unity 资源的优化管理.md
## 什么是 Asset
Mesh、Material、Texture、Audio、etc...

## 资源管理方式
- Assets

    - 只有被引用的资源会被打包

    - 适合存放静态资源

    - 不能动态加载

- Resources

    - 支持动态加载

    - Resources.assets 文件（2G限制）

    - 随安装包完全下载、无法动态更新

- StreamingAssets

    - 保持文件原始格式

    - 随安装包完全下载、无法动态更新

    - Application.streamingAssetsPath

- AssetBundle

    - Asset 的集合

    - 压缩（缺省）

    - 动态加载

    - 动态更新

*注：下面3个文件夹仍然放置在 Assets 文件夹中，这里的 Assets 指不包括下面3个文件夹的其他资源*