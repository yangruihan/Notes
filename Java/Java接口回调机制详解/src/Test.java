public class Test {
    public static void main(String argv[]) {
        Boss boss = new Boss();
        Employee employee = new Employee(boss);
        employee.doWork();
    }
}

// 回调接口
interface CallBack {
    public void doEvent();
}    

// 回调接口实现类，也可以是匿名内部类 
class Boss implements CallBack {
    public void doEvent() {
        System.out.println("工作已经完成，打电话汇报给老板！");
    }
}

// 控制类
class Employee {
    CallBack callBack;

    public Employee(CallBack callBack) {
        this.callBack = callBack;
    }

    public void doWork() {
        System.out.println("正在工作中...");
        System.out.println("工作即将完成...");
        callBack.doEvent();
    }
}

