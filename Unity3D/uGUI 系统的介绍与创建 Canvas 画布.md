# uGUI 系统的介绍与创建 Canvas 画布
## 与 onGUI 比较
- 独立的坐标体系

- 全新的事件机制

- 更加高效的运转效率

## 与 NGUI 比较
- 与 Unity 结合更紧密

- 自适应系统更完善

- 更方便的深度处理

- 省去了 Atlas，直接使用 Sprite Packer

## 三种 Render Space 渲染空间
- Screen Space - Overlay 屏幕最前端

- Screen Space - Camera 绑定摄像机

- World Space 世界空间

## 什么是 Atlas 图集
Atlas（图集）：把多张小图制作成一张大图，以节省图片所占用的空间

## Rect Transform 专有名词
|名词|解释|
|:---:|:---:|
|Pivot|中轴，中心点|
|Anchor|锚点|
|Width、Height|宽、高|
|PosX、PoxY|Pivot 与 Anchor 的距离|
|Stretch|拉伸|
|Left、Right、Top、Bottom|左右上下|

## UI Event 组件
- Event System 事件系统管理器

- Standalone Input Module 标准接收器

- Touch Input Module 触屏接收器

- Event Trigger 事件触发器

- Graphic Raycaster 界面组件的射线检测

- Physic/Physic2D Raycaster 场景物体的射线检测

## Event Trigger 事件接口类型
- Pointer 鼠标指针类

    - IPointerEnterHandler

    - IPointerExitHandler

    - IPointerDownHandler

    - IPointerUpHandler

    - IPointerClickHandler

- Drag & Drop 拖拽类

    - IBeginDragHandler

    - IInitializePotentialDragHandler

    - IDragHandler

    - IEndDragHandler

    - IDropHandler

- Select 点选类

    - IUpdateSelectedHandler

    - ISelectHandler

    - IDeselectHandler

- Input 输入类

    - IScrollHandler

    - IMoveHandler

    - ISubmitHandler

    - ICancelHandler