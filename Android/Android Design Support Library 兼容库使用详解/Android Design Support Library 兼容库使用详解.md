# Android Design Support Library 兼容库使用详解
参考资料:[Android Design Support Library 兼容库使用详解](http://www.jikexueyuan.com/course/2117.html)
## 1. 添加 Design Support Library 依赖
在 Android Studio 中，通过在`build.gradle(Module: app)`中的`dependencies`中添加：
`compile 'com.android.support:design:23.1.0'`（其中23.1.0为当前库版本号）

## 2. 常规控件
### 2.1 FloatingActionButton - 浮动的圆形按钮
**布局文件：**
```[xml]
<android.support.design.widget.FloatingActionButton/>
```
**常用属性：**
*注意：使用以下属性时，需要在Layout中添加命名空间：`xmlns:app="http://schemas.android.com/apk/res-auto"`*

|属性名|作用|
|-----|---|
| app:backgroundTint="" | 背景色 |
| app:fabSize="" | 包裹图片类型 |
| app:elevation="" | Z轴，用来控制控件阴影效果 |
| app:rippleColor="" | 按钮点击时颜色效果 |

### 2.2 TextInputLayout - 让 EditText 提示更加人性化
**布局文件：**
```[xml]
<android.support.design.widget.TextInputLayout>
    <EditText/>
</android.support.design.widget.TextInputLayout>
```

### 2.3 Snackbar - 可交互的提示框
**使用代码：**
```[java]
Snackbar snackbar = Snackbar.make(view, "提示文本", Snackbar.LENGTH_LONG | Snackbar.LENGTH_SHORT);
snackbar.show();
snackbar.setAction("按钮title", new View.OnClickListener(){
    @Override
    public void onClick(View v) {
        // do something
    }
});
```

## 3. 菜单控件
### 3.1 TabLayout - 便捷实现标签
**布局文件：**
```[xml]
<android.support.design.widget.TabLayout/>
```
**常用属性：**

|属性名|作用|
|-----|---|
| app:tabTextColor="" | 标签颜色 |
| app:tabSelectedTextColor="" | 标签选中时颜色 |
| app:tabIndicatorColor="" | 标签下标指示器颜色 |
| app:tabIndicatorHeight="" | 标签下标指示器高度 |
| app:tabMode="" | 标签能否进行横向滑动 |

**使用代码：**
```[java]
TabLayout tabs = (TabLayout) findViewById(R.id.tab_layout);
tabs.addTab(tabs.newTab().setText("Tab1"));
tabs.addTab(tabs.newTab().setText("Tab2"));
tabs.addTab(tabs.newTab().setText("Tab3"));
tabs.addTab(tabs.newTab().setText("Tab4"));
```

### 3.2 Navigation View - 美观的侧滑视图
**布局文件：**
```[xml]
<android.support.v4.widget.DrawerLayout
    xmlns:app="http://schemas.android.com/apk/res-auto">
    <include layout="@layout/activity_main"/>
    <android.support.design.widget.NavigationView>
    ...
    </android.support.design.widget.NavigationView>
</android.support.v4.widget.DrawerLayout>
```

**常用属性：**

|属性名|作用|
|-----|---|
| app:headerLayout="" | 头布局 |
| app:menu="" | 菜单布局 |
| android:layout_gravity="left" |在父视图中的布局|
| android:fitSystemWindows="true" |适应系统|

## 4. 具有过渡动画效果的布局 Layout
### 4.1 CoordinatorLayout - 完美协调子 View 工作的核心部件
**布局文件：**
```[xml]
<android.support.design.widget.CoordinatorLayout>
</android.support.design.widget.CoordinatorLayout>
```