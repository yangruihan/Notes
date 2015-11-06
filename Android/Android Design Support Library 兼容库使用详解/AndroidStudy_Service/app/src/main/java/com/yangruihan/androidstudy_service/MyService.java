package com.yangruihan.androidstudy_service;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class MyService extends Service {

    private boolean running = false;
    private String show = "Default String";
    private CallBack callBack = null;

    public MyService() {
    }

    // 绑定服务时，执行该方法
    @Override
    public IBinder onBind(Intent intent) {
        // TODO: Return the communication channel to the service.
        return new Binder();
    }

    // 当每次使用 startService 调用服务时，调用该方法（但一个服务同时只会存在一个实例）
    // 通过 Intent 传递的数据，也会在这里得到
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        System.out.println("onStartCommand");
        show = intent.getStringExtra("input");

        return super.onStartCommand(intent, flags, startId);
    }

    // 当服务开启，调用该方法
    @Override
    public void onCreate() {
        super.onCreate();
        System.out.println("onCreate");

        running = true;

        new Thread() {
            @Override
            public void run() {
                super.run();

                int i = 0;

                while (running) {
                    System.out.println(show + i);
                    i++;

                    // 执行回调函数，刷新 TextView 显示
                    if (callBack != null) {
                        callBack.onShowChanged(show + i);
                    }

                    try {
                        sleep(500);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }.start();
    }

    public void setCallBack(CallBack callBack) {
        this.callBack = callBack;
    }

    public CallBack getCallBack() {
        return callBack;
    }

    // 当服务销毁时，调用该方法
    @Override
    public void onDestroy() {
        super.onDestroy();
        System.out.println("onDestroy");

        running = false;
    }

    public static interface CallBack {
        void onShowChanged(String show);
    }

    public class Binder extends android.os.Binder {
        public void setShow(String s) {
            show = s;
        }
        public MyService getService() {
            return MyService.this;
        }
    }
}
