# Unity 3D 学习笔记
## 基本概念
### 场景和游戏对象

![](https://github.com/yangruihan/Notes/blob/master/Unity3D/image_res/u3d-1-1.jpg)

一个游戏项目由多个**场景**组成

在场景中，一个角色、一个模型、一个特效，都是游戏对象。场景就是由多个游戏**对象**组成的

每个游戏对象身上都会有多个**组件**，使游戏对象具备相应的特性、实现相应的功能

### 摄像机
在 Unity 中**摄像机**决定游戏最终的显示效果

同一个场景中允许存在**多个摄像机**

### 坐标系
- **世界坐标系（Global）**：是整个 3D 场景的坐标系

- **本地坐标系（Local）**：是某个游戏对象内独立的坐标系

### 网格
要想让模型显示出来，**网格**是必须的

### 纹理
使模型表面赋有细节，就需要给它添加**纹理**

### 材质和着色器
**材质**能够将纹理应用在模型上，**着色器**决定纹理呈现出的最终效果

### 工程与应用程序
- Assets 里面存放的是项目所需要的资源

- Library 里面存放的是所需要的库文件

- ProjectSettings 里面存放的是工程设置文件

- Temp 里面存放的是临时文件

### 组件开发
游戏物体想要实现什么功能，只需要添加对应的**组件**即可，我们可以在 Inspector 视图中查看当前游戏对象身上的组件，修改组件的属性

## 脚本生命周期

![](https://github.com/yangruihan/Notes/blob/master/Unity3D/image_res/u3d-1-2.jpg)

```csharp
// 这些方法定义了一个脚本从被加载到被销毁的过程中，脚本的生命周期
// 每当脚本被加载时调用，只会调用一次
// 在 Awake 中做一些初始化操作
void Awake ()
{
    // 初始化 public 成员
}

// 在每次激活脚本时调用
void OnEnable ()
{
}

// 在第一次调用 Update 之前调用，只会调用一次
// 在 Start 中也可以做一些初始化操作
void Start ()
{
}

// 每帧调用一次 Update
void Update ()
{
}

// 在 Update 方法调用完之后调用
void LateUpdate ()
{
}

// 脚本取消激活的时候调用 OnDisable
void OnDisable ()
{
}

// 脚本被销毁时调用一次
void OnDestroy ()
{
}

// 持续调用 - IMGUI 代码需要写在 OnGUI 方法中
void OnGUI ()
{
}

// 以固定的频率调用，不会受到图像刷新帧速率的影响
// 一般我们会把处理物理的代码放在这里
void FixedUpdate ()
{
}
```

## 获取用户输入
### 获取键盘输入
- `Input.GetKeyDown()`
- `Input.GetKeyUp()`
- `Input.GetKey()`

### 获取鼠标输入
- `Input.GetMouseButtonDown()`
- `Input.GetMouseButtonUp()`
- `Input.GetMouseButton()`

## GameObject