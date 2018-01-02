# 解决 Unity GameObject 层级关系变动导致 AnimationClip 中引用 Missing

今天在解决项目中某动画bug的时候遇到这样一个问题，特别记录一下：

有一个 GameObject，其层级关系及动画片段如下：

![](../image_res/animation_missing_1.png)

![](../image_res/animation_missing_2.png)

当我手动修改一个具有动画信息的 GameObject 的层级时，对应的动画信息中的引用会出现 Missing 问题，如下：

![](../image_res/animation_missing_3.png)

![](../image_res/animation_missing_4.png)

此时，通过文本格式打开该动画片段 xxx.anim（需要在 Edit -> Project Settings -> Editor 中把 Asset Serialization 改为 **Force Text**），可以看到形如下图的内容：

![](../image_res/animation_missing_5.png)

其中有一项 **path** 是我们需要用到的属性，将其改为 `A/CubeA`，如下：

![](../image_res/animation_missing_6.png)

![](../image_res/animation_missing_7.png)

刷新一下 Editor，发现动画片段已经恢复正常了。

*Tips*

- 修改可以通过脚本自动实现，无需每次都手动修改

- 可能会导致其他使用到该动画的地方出现异常，请谨慎使用