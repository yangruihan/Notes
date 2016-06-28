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
### 概念
场景中的每一个物体都是一个**游戏对象（GameObject）**

每一个游戏对象都有很多**组件**，**Transform组件**是每个游戏对象所必须有的

游戏对象更像一个容器，用来存放各种各样的组件，需要什么样的功能，就把相应功能的组件添加上去

### 代码操作演示
```csharp
// gameObject 获取当前脚本所挂载的游戏对象
// 一般来说，在属性视图中能够看到或修改的属性，我们同样可以在脚本中获取并修改

// 1. 游戏对象的名字
print (gameObject.name);
gameObject.name = "张三";

// 2. 获取游戏对象的 Tag
print (gameObject.tag);
gameObject.tag = "Player";

// 3. 游戏对象是否激活
print (gameObject.activeSelf);

// 4. 设置游戏对象的激活状态
gameObject.SetActive(false);

// 5. 获取游戏对象身上的组件
CubeController c = gameObject.GetComponent <CubeController> ();
print (c.controllerStr);

// 6. 给游戏对象添加指定类型的组件
Light l = gameObject.AddComponent <Light> ();

// 7. 通过 Tag 值查找游戏对象
GameObject g = GameObject.FindGameObjectWithTag ("Player");
print (g.name);

GameObject gg = GameObject.FindWithTag ("Player"); // 与上面效果一样

// 8. 通过游戏对象 name 来查找游戏对象
GameObject ggg = GameObject.Find ("Main Camera");
ggg.name = "主摄像机";

// 9. 销毁某个游戏对象
GameObject.Destroy (gg);

// 10. 通过 Tag 值来查找多个游戏对象
GameObject[] gs = GameObject.FindGameObjectsWithTag ("Player");
```

## Vector3——三维向量类

```csharp
// 新建一个向量
Vector3 v = new Vector3 ();
// 获取 v 向量的单位向量
v.Normalize ();                 // V 本身会变成长度为1，方向不变的单位向量
Vector3 no2 = v.normalized;     // V 本身不会发生改变，返回 V 方向上的单位向量
// 获取 V 向量的长度
float l = v.magnitude;

// 创建一个（0,1,0) 默认向量，Y轴正方向
Vector3 up = Vector3.up;
// 创建一个 (0,-1,0) 默认向量，Y轴负方向
Vector3 down = Vector3.down;

// 创建一个 (1,0,0) 默认向量，X轴正方向
Vector3 right = Vector3.right;
// 创建一个 (-1,0,0) 默认向量，X轴负方向
Vector3 left = Vector3.left;

// 创建一个 (0,0,1) 默认向量，Z轴正方向
Vector3 forward = Vector3.forward;
// 创建一个 (0,0,-1) 默认向量，Z轴负方向
Vector3 back = Vector3.back;

// 创建一个 (0,0,0) 默认向量
Vector3 zero = Vector3.zero;

Vector3 v1 = new Vector3 (1f, 1f, 1f);
Vector3 v2 = new Vector3 (-1f, 1f, -1f);
// 求两个向量的夹角
float angle = Vector3.Angle (v1, v2);

// 求两个点的距离
float dis = Vector3.Distance (v1, v2);

// 向量点乘
float res = Vector3.Dot (v1, v2);

// 向量×乘
Vector3 res2 = Vector3.Cross (v1, v2);
```

## Transform组件
- 控制游戏对象的位置、旋转、缩放

- 管理游戏对象间的父子关系

```csharp
// 获取当前脚本所挂载的游戏对象身上的 Transform 组件
// transform

// 1. 控制游戏对象的位置、旋转、缩放

// position 属性 - 世界坐标系中的位置
Vector3 position = transform.position;
print (position);

// localPosition 属性 - 局部坐标系中的位置
Vector3 localPosition = transform.localPosition;
print (localPosition);

// 一般不直接修改四元数
// transform.rotation;
// transform.localRotation;

// localScale - 控制游戏对象的缩放
Vector3 localScale = transform.localScale;

// 变换当前游戏对象，使游戏对象产生移动的方法
transform.Translate (new Vector3(0f, 1f, 0f));
// 变换当前游戏对象，使游戏对象产生旋转
transform.Rotate (Vector3.up, 10f);
// 使用欧拉角进行旋转
transform.eulerAngles = new Vector3 (0f, 45f, 0f);

// 2. 控制游戏对象间的父子关系

// 获取/重新制定 当前对象父对象的 Transform 组件
Transform parent = transform.parent;

// 获取当前对象的根对象的 Transform 组件
Transform root = transform.root;

// 通过名字来获得对象的子对象
Transform child = transform.Find ("Cube");
Transform child2 = transform.FindChild ("Cube");
```

## 常用工具类
### Time 类

```csharp
// Time 类用来进行时间控制

// 获取从游戏开始到当前帧，所消耗的总时长，单位为 s
float t = Time.time;
print (t);

// 获取从上一帧开始到当前帧结束，所消耗的总时长，单位 s
float delta = Time.deltaTime;
print (delta);

// 表示时间流逝的快慢
// 1 表示正常时间流逝
// 2 表示二倍速时间流逝
// 0 表示时间停止，游戏暂停
float ts = Time.timeScale;
```

### Mathf 类

```csharp
// Mathf 类用来进行数学计算

// 求绝对值
int i = Mathf.Abs (-12); // i = 12
// 求最大最小值
int max = Mathf.Max (1, 5, 2, 4); // max = 5
int min = Mathf.Min (1, 5, 2, 4); // min = 1
// 三角函数
float sin = Mathf.Sin (0.5f);
float cos = Mathf.Cos (0.5f);
float pi = Mathf.PI;
// 求平方根
float res = Mathf.Sqrt (4);
```

## 预设体
- 预设体能够使游戏对象和资源重复使用

- 相同的游戏对象可以使用同一个预设体来创建

- 对预设体进行修改后，所有游戏对象都会相应改变

```csharp
// 利用预设体随机生成 GameObject 对象代码段
Vector3 v = new Vector3 ();
v.y = 0.5f;
v.x = Random.Range (-5f, 5f);
v.z = Random.Range (-5f, 5f);

Quaternion q = Quaternion.AngleAxis (Random.Range (0, 360f), transform.up);

// 根据给定的 GameObject 生成其的克隆实例
Instantiate (playerPrefab, v, q);
```

## 鼠标事件
- `OnMouseDown()`

- `OnMouseEnter()`

- `OnMouseDrag()`

- `OnMouseOver()`

- `OnMouseUp()`

- `OnMouseExit()`

- `OnMouseUpAsButton()`

```csharp
// 鼠标事件，直接在类中重写以下方法即可

// 1. 当鼠标点击下去游戏对象时，调用该方法
void OnMouseDown () {
    print ("Down");
}

// 2. 当鼠标点击松开游戏对象时，调用该方法
void OnMouseUp () {
    print ("Up");
}

// 3. 当鼠标点击下去还未松开游戏对象时，调用该方法
void OnMouseDrag () {
    print ("Drag");
}

// 4. 当鼠标移动到游戏对象内部时，调用该方法
void OnMouseEnter () {
    print ("Enter");
}

// 5. 当鼠标移出游戏对象内部时，调用该方法
void OnMouseExit () {
    print ("Exit");
}

// 6. 当鼠标持续停留在游戏对象内部时，调用该方法
void OnMouseOver () {
    print ("Over");
}

// 7. 当我们像点击按钮一样点击游戏对象，即鼠标松开时仍然在游戏对象内部时，调用该方法
void OnMouseUpAsButton () {
    print ("UpAsButton");
}
```

## 物理引擎
### 简介
- 物理引擎能够真实的模拟物理效果

- Unity 中使用的是 NVIDIA 的 PhysX 物理引擎

- 在 Unity 中使用 Rigidbody 让游戏对象受物理引擎控制

### 属性
|属性名|作用|
|:---:|:---:|
|Mass|质量，主要用于控制惯性|
|Drag|空气阻力，当游戏对象发生位移时受到的空气阻力大小|
|Angular Drag|角空气阻力，当游戏对象发生角度变化时受到的空气阻力大小|
|Use Gravity|是否使用重力|
|Is Kinematic|是否使用运动学，使用后则需要通过运动学来计算位移，而不受力的影响|
|Interpolate|插值方式，默认为 None，有 Interpolate 内插值和 Extrapolate 外插值|
|Collision Detection|碰撞检测方式|
|Constraints|约束，当设置约束后，相应的值不会发生改变|

### 方法

- `AddForce()` 施加力

- `AddExplosionForce()` 施加爆炸力

- `AddTorque()` 施加力矩

- `AddForceAtPosition()` 在指定位置施加力

## Colliser 组件

- Colliser 组件的主要功能是进行碰撞检测

- 使用刚体时，一般都会和 Colliser 共同使用

## 碰撞事件
- 发生碰撞的两个物体都必须带有 Colliser

- 发生碰撞的两个物体至少有一个带有刚体

- 发生碰撞的两个物体必须有相对运动

```csharp
// 碰撞相关的三个事件，直接在类中重写以下方法即可

// 碰撞开始时会调用一次
void OnCollisionEnter (Collision other) {
    print ("碰撞开始");
}

// 碰撞结束时会调用一次
void OnCollisionExit (Collision other) {
    print ("碰撞结束");
}

// 当碰撞持续发生的时候调用
void OnCollisionStay (Collision other) {
    print ("碰撞进行中");
}
```

## 触发器事件
- 发生碰撞的两个物体都必须带有 Colliser，并且至少其中一个物体的 Is Trigger 选项是勾选的

- 发生碰撞的两个物体至少有一个带有刚体

```csharp
// 触发器相关的三个事件

// 刚刚进入触发范围时，会调用一次
void OnTriggerEnter (Collider other) {
    print ("进入触发范围");
}

// 持续呆在触发范围内，会一直调用
void OnTriggerStay (Collider other) {
    print ("持续触发");
}

// 离开触发范围时，会调用一次
void OnTriggerExit (Collider other) {
    print ("离开触发范围");
}
```