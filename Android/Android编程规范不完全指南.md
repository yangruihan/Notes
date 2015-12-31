# Android编程规范不完全指南
参考资料：[我总结的Android编程规范](http://www.cnblogs.com/wangqiguo/p/4770228.html)

## 1. 命名规则
### 1.1 类名，接口名：
以大写开头，如果一个类的类名由多个单词组成，所有单词的首字母必须大写，单词尽量写全称，不要简写，除非约定俗成的名字，例如：URL，RTMP，RTSP 这些广泛使用的专有名词，可以全部大写，也可以首字母大写。

例如：`HttpRequest`，`CourseActivity`

### 1.2 局部变量，类的成员变量，类的成员函数，函数参数：
以小写字母开头其他的单词首字母大写，变量名不建议使用下划线分隔单词，建议使用驼峰命名法，Android 的系统类都采用此方法。

例如：`toString()`，`onCreateView(Bundle saveInstanceState)`

### 1.3 静态常量：
常量单词全部大写，所以单词之间使用下划线分隔。

例如：`WHAT_EMPTY_CONTENT`

### 1.4 控件变量的命令，控件的ID命名：
建议：xml 布局文件中的控件的id的命令与*.java的代码文件中的空间对象的命名保持一致。
```[java]
class MyActivity extends Activity {
    TextView txtUserName;
    ...
    void onCreate(Bundle saveInstanceState) {
        txtUserName = (TextView) findViewById(R.id.txtUserName);
    }
}
```

### 1.5 常用控件以及类对象命名的规范说明：

|类名|变量名|类名|变量名|
|----|------|----|------|
|TextView|txtDescription|ProgressBar|progressDescription|
|Button|btnDescription|SeekBar|seekBarDescription|
|ImageButton|imgBtnDescription|VideoView|vvDescription|
|ImageView|imgDescription|Spinner|spinDescription|
|RadioButton|rbDescription|WebView|webViewDescription|
|EditText|editDescription|ListView|listViewDescription|
|ScrollView|scrollDescription|GridView|gridDescription|
|Handler|descriptionHandler|RatingBar|ratingBarDescription|
|PullToRefreshListView|pullRefreshViewDescription|Adapter|descriptionAdapter|
|Fragment|descriptionFragment|Activity|descriptionActivity|
|List<T>|descriptionList|Map<>|mapDescription|
|SlidingMenu|slidMenuDescription|ViewPager|viewPagerDescription|
|CheckBox|chBoxDescription|View|viewDescription|
|RadioGroup|rgDescription|ExpandableListView|expDescription|
|FrameLayout|frameLayDescription|SharedPreferences|spDescription|
|LinearLayout|lineLayDescription|RelativeLayout|relativeLayDescription|
|startActivityForResult(requestCode)|REQUEST_CODE_DESCRIPTION|msg.what|WHAT_DESCRIPTION|

### 1.6 资源命名：
1. `layout`资源文件的命名（全部小写，下划线分隔）：
    - `activity`资源文件：activity_description1_description2.xml
    - `fragment`资源文件：fragment_description1_description2.xml
    - `listview`列表项资源文件：list_item_description1_description2.xml
    - 可复用（被include）的组件资源文件：control_description1_description2.xml
    - `drawable`资源：controlName_description1_description2_selector.xml
        - `controlName`表示该资源要用在什么类型的控件上，例如：如果是按钮的图片切换则应该命名为`button_bg_sendmessage_selector.xml`
        - `selector`表示该资源的形式，例如还有`shape`等
2. 图片资源的命名：同上
3. 颜色值的命名：`color_description`以`color`为前缀，全部小写，下划线分割。
`description`既可以是该颜色使用的功能描述，也可以是该颜色的英文描述，也可以是具体的颜色值，例如：
```[xml]
<color name="color_white">#ffffff</color>
<color name="color_grey_ccc">#cccccc</color>
<color name="color_grey_ddd">#dddddd</color>
```
因为grey可能有很多的等级，有时候需要不同等级的灰色，没有那么多英文名可以区分，所以名字中可以直接使用颜色值
```[xml]
<color name="color_button_pressed">#4c4c4c</color>
```
根据功能定义description，表示该颜色用于按钮按下

*注：不允许出现毫无意义的命名，例如 textview1, textview2*

## 2. 关于字面常量
代码中不允许出现直接硬编码的字面常量，如果是控件上显示的文本，必须放在`string.xml`资源文件中。如果是代码中用到的常量字符串，必须定义成`public static final String`类型的常量值，在代码中使用该定义的常量值。

这样做的好处是以后需要修改该常量值，只需要修改一个地方。如果是硬编码在代码中则需要修改所有使用它的地方，而且拷贝容易出错。

在`Activity`之间传递参数的时候，`intent.putExtra`的`key值`也要符合命名规范，并且统一定义为静态常量，不能直接硬编码在代码中，否则想要修改的时候非常麻烦。某一个`Activity`在被启动的时候需要接受参数，那么这些参数的`key`定义就应该放在该`Activity`中。

## 3. JSON解析
Android中调用服务器端的接口一般返回的是`JSON`数据，在解析`JSON`的时候，无论是使用原始的手工解析方式，还是使用`javabean`的解析方式，解析出来的结果在使用的时候必须都进行判空处理。不允许因为服务端的`json`出问题，导致app在解析`json`的时候出现崩溃。

## 4. 类成员初始化
所有类的成员变量一定要赋初始值，不允许只定义，不赋值。

## 5. Int类型常量
函数返回的时候，如果返回的int类型的数据并不是真实的实用的数据值（例如表示宽度、高度、大小等值），仅仅代表函数执行成功、失败、异常的状态值，并且这些值是有限的几个值，必须要将这些值使用静态常量描述，或者使用枚举类型，例如：
```[java]
int GetJsonString()
```
该函数返回-1表示获取解析`JSON`数据异常，返回0表示成功，返回1表示网络连接异常，返回2表示`JSON`内容中的数据部分为空。

那么在函数内部的代码里不要直接使用这些字面值，这些字面值对于程序员来说是毫无意义的，代码可阅读性很差，建议做成下面的模式：
```[java]
public static final int RESULT_PARSE_JSON_EXCEPTION = -1;
public static final int RESULT_SUCCESS = 0;
public static final int RESULT_NETWORK_EXCEPTION = 1;
public static final int RESULT_NO_DATA = 2;
``` 
使用这些符号常量值代替字面值的好处是，符号常量值是由大写的英文单词组成，是有意义的，可以帮助程序员更好的理解函数返回值的意义，而且符号常量值对应的具体的赋值在后期也是很方便修改的。

## 6. Activity 接受参数与模块化
如果一个`Activity`可能在多个地方被打开，或者一个`Fragment`可能在多个地方被用到。那么在设计该`Activity`和`Fragment`的时候一定要考虑低耦合，对外提供统一的参数接口，启动`Activity`的过程封装在该`Activity类`的静态成员方法里。

类似如下：
```[java]
class MyActivity extends Activity {
    ...
    public static void startActivity(Context context, Params param) {
        Intent intent = new Intent(context, MyActivity.class);
        intent.putExtra("param", param);
        startActivity(intent);
    }

    public static void startActivityForResult(Context context, Params param) {
        Intent intent = new Intent(context, MyActivity.class)
        intent.putExtra("param", param);
        startActivityForResult(intent, REQUEST_CODE);
    }
}
```
参数的传递最好是封装在一个`Model实体中`，避免使用`Map`这种方式进行参数传递。建议该实体类实现为对应的`Activity`的静态可序列化的内部类。

## 7. Android Studio 工程目录组织
Android Studio 中的项目的包结构应该根据工程各个部分的功能来组织。

## 8. Handler 的封装
每个`Activity`里面几乎都会定义一个`Handler内部类`，但是很多`Activity`里面的`Handler`都使用了重复的消息类型，这里面是有冗余代码的，所以应该把这些`Activity`都是用到的`Handler`类的消息部分，提取成一个公共的`Handler`类。

然后在各个`Activity`里面使用继承的方式，来提供该`Activity`特有的`Handler`消息类型的`Handler`类实现。

另外`Handler`发送消息应该使用`Handler`类的成员函数，不应该直接使用`handler.obtainMessage(xxx).sendToTarget();`这种原始的发送消息的方式，这样不利于降低耦合，这种细节应该隐藏在`Handler`类的里面。

`Handler`的消息类型应该定义为`Handler`类里面的静态常量，而该常量不应是`public`的，对外部不可见。也就是说使用`handler`对象发送消息的细节不应该暴露给外部。

## 9. List 数据更新
封装`ListView`的数据更新，在`handlerMessage`中更新数据，避免出现`java.lang.IllegalStateException`问题

## 10. Activity 与 Fragment 之间传递参数
`Activity`与`Fragment`的数据传递采用`interface`的方式，也就是代理的方式，这样可以降低耦合，有利于`Fragment`的复用：
在你的`fragment`中定义一个接口：
```[java]
public interface OnDataPass {
    public void onDataPass(String data);
}
```
然后，在你的`fragment`中`onAttach(Activity a)`方法中实例化一个`OnDataPass`：
```[java]
OnDataPass dataPasser;

@Override
public void onAttach(Activity a) {
    super.onAttach(a);
    dataPasser = (OnDataPass) a;
}
```
在你的`fragment`中，当你想处理传递的数据时，只需要调用`dataPasser`的`onDataPass`方法即可：
```[java]
public void passData(String data) {
    dataPasser.onDataPass(data);
}
```
最后，在你的容器`activity`中实现`OnDataPass`接口：
```[java]
public class MyActivity extends Activity implements OnDataPass {
    ...
    @Override
    public void onDataPass(String data) {
        Log.d("LOG", "Hello " + data);
    }
}
```

## 11. 网络请求数据模块化
一般在`Activity`中我们通过网络请求服务端的接口获得数据，这个过程一般是在一个线程中做的，获取到数据之后，再通过`Activity`中的`handler`发送消息来通知`Activity`更新数据。

该负责获取数据的线程类，我们一般都实现为一个`Activity`的内部类，该类可以直接访问`Activity`的成员变量，例如`handler`，数据列表对象等。但是这样不利于该数据获取线程的复用。如果另一个`Activity`里面也需要获取相同的数据，那么这个功能是不能复用的。

所以这个负责数据请求的线程类，不应该与具体的`Handler`和`Activity`联系过于紧密。应该定义为一个静态类，`handler`应该作为参数传递进来，而不是直接访问外部类的成员变量。

## 12. 封装 Log 功能
`Log`功能应该封装成为自动将当前所在类的类名变成log输出的TAG参数，发布的app最好是能循环写日志文件到系统存储中，并且日志文件应该使用反复覆盖的方式重复利用。

下面仅仅是一个不完善的例子：
```[java]
public class MyLog {
    public static final String TAG = "myapp ";
    public static void v(Object o, String message) {
        Log.v(TAG + o.getClass().getSimpleName(), message);
    }
}
```
使用方法：
```[java]
MyLog(this, "Hello Log");
```
打印结果
```
V/myapp MainActivity: Hello Log
```

## 13. 版本控制
使用自动化版本管理，自动生成版本号，使应用程序的版本与版本库上保持一致。

使用hg替换工程目录下的app目录下的`build.gradle`文件即可，如果`AndroidManifest.xml`里面也有版本号的设置，AndroidStudio还是以`build.gradle`为准。不应该在每次发布的时候，在AndroidStudio的工程设置里面手工修改版本号。


## 14. 为程序添加全局异常捕获
应该为app添加全局异常捕获，app中总会有一些我们未捕获的异常，一旦用户使用过程中遇到这样的异常，程序就会崩溃，我们应该检测该类未捕获的异常信息，程序崩溃的时候通过写文件日志，或者发送邮件的方式获得异常信息，以便解决bug。
