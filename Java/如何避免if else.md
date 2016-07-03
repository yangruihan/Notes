# 如何避免if else
在开发的过程中相信你也会写很多的if else语句吧，此篇主要来讲讲如何在日常开发的过程中尽量少的使用if else语句

## 为什么要去if else
在开发的过程中我们可能会经常遇到if else的逻辑，写很多if else对于一位有情怀的程序员看来是不可以接收的，也影响阅读人的阅读感受，同时程序也违背了对修改关闭扩展开放的原则。在写程序的过程中我们应该尽量保证修改关闭，也就是说自己的写的代码逻辑应不该让别人在扩展逻辑的过程中进行修改，同时保证高的可扩展性

在使用if else写程序的过程中你可能会写出如下的代码：

```java
String strategy = "";
if(strategy.equals("策略一")){

}else if(strategy.equals("策略二")){

}else if(...){

}else {

}
```

当需要加一个分支逻辑就必须得去if else结构中改代码，这样不利于程序扩展，同时也非常难维护，如果业务复杂到一定的程度这块代码可能没法去重构了

## 策略模式 + 工厂模式 + 单例模式
在想到要去掉if else这种结构来写代码，首先容易想到的是通过一个map来保存key对应的逻辑，作为一个从c++转到java开发的程序的思维是通过函数指针来解决问题，但是java并没有这么个东西，所以就有了下面这段代码逻辑了

```java
public class StrategyTest {

    public static void main(String[] args) {
        String result = ObtainStrategyInfo.getInstance().getStrategyInfo("策略一");
        System.out.println(result);
    }
}

/* （单例模式） */
class ObtainStrategyInfo {

    private static final ObtainStrategyInfo obtainStrategyInfo = new ObtainStrategyInfo();

    public static ObtainStrategyInfo getInstance(){
        return obtainStrategyInfo;
    }

    public String getStrategyInfo(String strategy){
        StrategyInfo strategyInfo = new StrategyFactory().getStrategyInfoClass(strategy);
        return strategyInfo.getStrategyInfo(strategy);
    }
}
这种单例模式在类一加载的时候就将单例对象创建完毕，总是这个对象存在内存中，避免了通过线程同步来生成对象，线程安全的创建方式。

/* 其实最终的if else判断逻辑都在这里了 （工厂模式）*/
class StrategyFactory {
    private static Map<String, StrategyInfo> strategyInfoMap = new HashMap<String, StrategyInfo>();

    static {
        strategyInfoMap.put("策略一", new Strategy1());
        strategyInfoMap.put("策略二", new Strategy2());
    }

    public StrategyInfo getStrategyInfoClass(String strategy){
        StrategyInfo strategyInfo = null;
        for(String key : strategyInfoMap.keySet()){
            if(strategy.equals(key)) {
                strategyInfo = strategyInfoMap.get(key);
            }
        }
        return strategyInfo;
    }
}

/* (策略模式) */
interface StrategyInfo {
    String getStrategyInfo(String strategy);
}

class Strategy1 implements StrategyInfo {

    public String getStrategyInfo(String strategy) {
        return strategy;
    }
}

class Strategy2 implements StrategyInfo {

    public String getStrategyInfo(String strategy) {
        return strategy;
    }
}
```

如果需要扩展策略三，是不是只要添加自己的逻辑代码就行了呢？保证对修改关闭？答案是肯定的。可以如下方式来扩展策略三：

```java
/* 在StrategyFactory中注入策略三的对应关系 */
strategyInfoMap.put("策略三", new Strategy3());

/* 然后定义策略三 */
class Strategy3 implements StrategyInfo {

    public String getStrategyInfo(String strategy) {
        return strategy;
    }
}
```

这样可非常方便的扩展策略四、策略五

