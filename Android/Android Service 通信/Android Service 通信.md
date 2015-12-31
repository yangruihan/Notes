# Android Service 通信
参考资料：[Android 中 Service 通信](http://www.jikexueyuan.com/course/715.html)

GitHub演示项目：[Android Service 通信](https://github.com/yangruihan/Notes/tree/master/Android/Android%20Service%20%E9%80%9A%E4%BF%A1/AndroidStudy_Service)

## 1. 创建一个 Service
### 1.1 创建 Service 类
新建一个类，这里取名 MyService，继承自 Service
```[java]
public class MyService extends Service {
    // 构造方法
    public MyService() {
    }

    // 绑定服务时，执行该方法
    @Override
    public IBinder onBind(Intent intent) {
        // TODO: Return the communication channel to the service.
        throw new UnsupportedOperationException("Not yet implemented");
    }

    // 当服务开启时，调用的方法
    @Override
    public void onCreate() {
        super.onCreate();
    }

    // 当服务销毁时，调用的方法
    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 当每次使用 startService 调用服务时，调用该方法（但一个服务同时只会存在一个实例）
     * 通过 Intent 传递的数据，也会在这里得到
     * 注意和 onCreate() 方法的区别：
     *      onCreate() 一个应用开启服务时，该方法只会调用一次
     *      onStartCommand() 只要使用 startService() 调用此服务，该方法就会执行一次
     */
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return super.onStartCommand(intent, flags, startId);
    }
}
```

### 1.2 在 AndroidManifest.xml 中注册该服务
```[xml]
<service
    android:name=".MyService"
    android:enabled="true"
    android:exported="true" >
</service>
```
在 AndroidManifest.xml 里 Service 元素的常见选项：

|选项名|作用|
|-----|---|
|android:name|服务类名|
|android:label|服务的名字，如果此项不设置，那么默认显示的服务名则为类名|
|android:icon|服务的图标|
|android:permission|申明此服务的权限，这意味着只有提供了该权限的应用才能控制或连接此服务|
|android:process|表示该服务是否运行在另外一个进程，如果设置了此项，那么将会在包名后面加上这段字符串表示另一进程的名字|
|android:enabled|如果此项设置为 true，那么 Service 将会默认被系统启动，不设置默认此项为 false|
|android:exported|表示该服务是否能够被其他应用程序所控制或连接，不设置默认此项为 false|

## 2. 开启|关闭 Service
### 2.1 通过 startService() | stopService 方法开启和关闭服务
**开启服务代码如下：**
```[java]
// 新建一个 Intent
Intent intent = new Intent(this, MyService.class);
// 这里可以给 Intent 里面放置数据，数据对应在 Service 类中 onStartCommand 方法参数 Intent 可以得到
// 开启服务
startService(intent);
```
**关闭服务代码如下：**
```[java]
// 新建一个 Intent
Intent intent = new Intent(this, MyService.class);
// 关闭服务
stopService(intent);
```

### 2.2 通过 bindService() | unbindService() 方法开启和关闭服务
**开启服务代码如下：**
```[java]
// 新建一个 Intent
Intent intent = new Intent(this, MyService.class);
// 开启服务
bindService(i, this, BIND_AUTO_CREATE);
```
此时，Service 要修改onBind() 方法，返回一个实现 IBinder 接口的对象：
```[java]
@Override
public IBinder onBind(Intent intent) {
    // Android 官方提供的 Binder 对象，不过为了实现 Service 和 Activity 互相通信，之后我们要自己重新实现一个类
    return new Binder();
}
```
并且，Activity 中需要实现 ServiceConnection 接口中 onServiceConnected() 和 onServiceDisconnected() 两个方法，这两个方法也是 Service 和 Activity 相互通信的关键，代码如下：
```[java]
// 服务连接时，调用该方法，其中 IBinder 对象，就是上面 onBind() 方法中返回的对象
@Override
public void onServiceConnected(ComponentName name, IBinder service) {
}

// 服务断开连接时，调用该方法
@Override
public int onStartCommand(Intent intent, int flags, int startId) {
    return super.onStartCommand(intent, flags, startId);
}
```
**关闭服务代码如下：**
```[java]
unbindService(this);
```

## 3. Activity 和 Service 通信
### 3.1 Activity -> Service 通信
#### 3.1.1 通过 Intent 通信
该方法是在开启服务时通过将数据放置到 Intent 中，传递给 Service，代码如下：
```[java]
Intent intent = new Intent(this, MyService.class);
// 为服务传递数据
intent.putExtra("input", "data");
startService(intent);
```
Service 则可以从 onStartCommand(Intent intent, int flags, int startId) 参数 intent 可以得到相信数据， 代码如下：
```[java]
@Override
public int onStartCommand(Intent intent, int flags, int startId) {
    String data = intent.getStringExtra("input");

    return super.onStartCommand(intent, flags, startId);
}
```

#### 3.1.2 通过 Binder 通信
首先在 Service 类中新建一个内部类 Binder 并继承自 Android.os.Binder，在其中实现获得数据的方法，代码如下：
```[java]
public class Binder extends android.os.Binder {
    public void setData(String d) {
        // data 为 Service 类的属性，在内部类中可以很方便的访问到外部的属性
        data = d;
    }
}
```
接着，在 Activity 中添加一个 Binder 属性，并通过 onServiceConnected(ComponentName name, IBinder service) 方法的参数对其进行赋值，代码如下：
```[java]
private MyService.Binder binder = null;
@Override
public void onServiceConnected(ComponentName name, IBinder service) {
    // 这里要强制类型转换一下
    binder = (MyService.Binder) service;
}
```
最后，便可以在想对 Service 属性修改的地方调用之前设置的 set 方法，代码如下：
```[java]
if (binder != null) {
    binder.setData("123");
}
```
系统会在 Service 启动时，对 binder 进行赋值，成功启动后，便可以顺利地实现数据传递

### 3.2 Service -> Activity 通信
要想实现 Service 对 Activity 的通信，需要用到回调函数
首先在 Service 中新建一个接口，代码如下：
```[java]
public static interface CallBack {
    void getServiceData(String data);
}
```
其次在 Service 中新建一个 CallBack 属性，并设置 set、get 方法，代码如下：
```[java]
private CallBack callBack = null;
public void setCallBack(CallBack callBack) {
    this.callBack = callBack;
}

public CallBack getCallBack() {
    return callBack;
}
```
然后在需要传值的地方调用接口方法，代码如下：
```[java]
// 需要传值的地方
if (callBack != null) {
    // 将需要传递的值作为参数
    callBack.getServiceData(data);
}
```
接着在 Activity 中为 Service 设置 CallBack 并实现 getServiceData() 方法，那么怎么获得 Service 对象呢？
我们仍然可以通过 Binder 来获得 Service 对象，在 Binder 类中添加如下方法：
```[java]
public MyService getService() {
    return MyService.this;
}
```
在 Activity 中的 onServiceConnected() 方法中获得 Service 对象，并设置 CallBack，代码如下：
```[java]
@Override
public void onServiceConnected(ComponentName name, IBinder service) {
    binder = (MyService.Binder) service;
    binder.getService().setCallBack(new MyService.CallBack() {
        @Override
        public void getServiceData(String data) {
            // 这里的 data 参数便是从 Service 传递过来的数据
        }
    });
}
```