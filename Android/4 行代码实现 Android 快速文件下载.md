# 4 行代码实现 Android 快速文件下载
文章[原地址](http://www.codeceo.com/article/android-download-4-line-code.html)

## 简单使用
完成一个下载任务只需要4行代码，什么断点续传，大文件下载，通知栏进度显示….都不需要你操心

```java
//创建下载任务,downloadUrl就是下载链接
DownloadManager.Request request = new DownloadManager.Request(Uri.parse(downloadUrl));
//指定下载路径和下载文件名
request.setDestinationInExternalPublicDir("/download/", fileName);
//获取下载管理器
DownloadManager downloadManager= (DownloadManager) mContext.getSystemService(Context.DOWNLOAD_SERVICE);
//将下载任务加入下载队列，否则不会进行下载
downloadManager.enqueue(request);
```

## 高级用法
- 通过上面的代码大家也看出来了，我们使用的是系统提供的下载管理器进行下载，从API 9就开始支持了，所以不用担心在兼容问题

- 既然是系统提供的，那么肯定还有更加强大的用法，文章继续

让我们看DownloadManager的源码，提供了这么多方法

DownloadManager的方法

![](http://static.codeceo.com/images/2016/06/a9078e8653368c9c291ae2f8b74012e79.jpg)

DownloadManager.Request的方法

![](http://static.codeceo.com/images/2016/06/960283-b2026b07e3a86194.png)

方法差不多就这些，已经比较全了，可以满足我们绝大部分的使用场景

## 实际使用
接下来我们就以APP应用内更新为例，讲一下这些方法的使用

1. 首先我们梳理下APP应用内更新的逻辑

    ![](http://static.codeceo.com/images/2016/06/a9078e8653368c9c291ae2f8b74012e711.jpg)

2. 接下来看具体实现，上代码

    ```java
    //使用系统下载器下载
    private void downloadAPK(String versionUrl, String versionName) {
        //创建下载任务
        DownloadManager.Request request = new DownloadManager.Request(Uri.parse(versionUrl));
        request.setAllowedOverRoaming(false);//漫游网络是否可以下载

        //设置文件类型，可以在下载结束后自动打开该文件
        MimeTypeMap mimeTypeMap = MimeTypeMap.getSingleton();
        String mimeString = mimeTypeMap.getMimeTypeFromExtension(MimeTypeMap.getFileExtensionFromUrl(versionUrl));
        request.setMimeType(mimeString);

        //在通知栏中显示，默认就是显示的
        request.setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE);
        request.setVisibleInDownloadsUi(true);

        //sdcard的目录下的download文件夹，必须设置
        request.setDestinationInExternalPublicDir("/download/", versionName);
        //request.setDestinationInExternalFilesDir(),也可以自己制定下载路径

        //将下载请求加入下载队列
        downloadManager = (DownloadManager) mContext.getSystemService(Context.DOWNLOAD_SERVICE);
        //加入下载队列后会给该任务返回一个long型的id，
        //通过该id可以取消任务，重启任务等等，看上面源码中框起来的方法
        mTaskId = downloadManager.enqueue(request);

        //注册广播接收者，监听下载状态
        mContext.registerReceiver(receiver,
                new IntentFilter(DownloadManager.ACTION_DOWNLOAD_COMPLETE));
    }
    ```

    接下来是广播接收器

    ```java
    //广播接受者，接收下载状态
    private BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            checkDownloadStatus();//检查下载状态
        }
    };
    ```

    检查下载状态

    ```java
    //检查下载状态
    private void checkDownloadStatus() {
        DownloadManager.Query query = new DownloadManager.Query();
        query.setFilterById(mTaskId);//筛选下载任务，传入任务ID，可变参数
        Cursor c = downloadManager.query(query);
        if (c.moveToFirst()) {
            int status = c.getInt(c.getColumnIndex(DownloadManager.COLUMN_STATUS));
            switch (status) {
                case DownloadManager.STATUS_PAUSED:
                    MLog.i(">>>下载暂停");
                case DownloadManager.STATUS_PENDING:
                    MLog.i(">>>下载延迟");
                case DownloadManager.STATUS_RUNNING:
                    MLog.i(">>>正在下载");
                    break;
                case DownloadManager.STATUS_SUCCESSFUL
                    MLog.i(">>>下载完成");
                    //下载完成安装APK
                    //downloadPath = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).getAbsolutePath() + File.separator + versionName;
                    installAPK(new File(downloadPath));
                    break;
                case DownloadManager.STATUS_FAILED:
                    MLog.i(">>>下载失败");
                    break;
            }
        }
    }
    ```

    安装APK

    ```java
    //下载到本地后执行安装
    protected void installAPK(File file) {
        if (!file.exists()) return;
        Intent intent = new Intent(Intent.ACTION_VIEW);
        Uri uri = Uri.parse("file://" + file.toString());
        intent.setDataAndType(uri, "application/vnd.android.package-archive");
        //在服务中开启activity必须设置flag,后面解释
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mContext.startActivity(intent);
    }
    ```

    到此就完成了应用内更新APP的代码，但是有一些坑需要注意！

## 希望你可以看到最后这几句，不然你也会被坑的！
1. 虽然下载什么的不需要自己操心了，但是建议还是将整个上面四段代码放在 Service 中执行，因为放在 Activity 中时，当用户按 home 键后，即使下载完了，也不会弹出安装界面

2. 建议使用`startService的`方式启动 Service，这样不会与 Activity 生命周期绑定，保证下载完后能顺利安装

3. Service使用完后要及时地停掉