# Java 动态代理（JDK 和 cglib）[转]
[原文](http://www.cnblogs.com/jqyp/archive/2010/08/20/1805041.html)

## 代理模式
代理模式是常用的 Java 设计模式，它的特征是代理类与委托类有同样的接口，代理类主要负责为委托类预处理消息、过滤消息、把消息转发给委托类，以及事后处理消息等。代理类与委托类之间通常会存在关联关系，一个代理类的对象与一个委托类的对象关联，代理类的对象本身并不真正实现服务，而是通过调用委托类的对象的相关方法，来提供特定的服务。

## 代理的分类
1. 静态代理
    
    由程序员创建或特定工具自动生成源代码，再对其编译。在程序运行前，代理类的`.class`文件就已经存在了。

2. 动态代理
    
    在程序运行时，运用反射机制动态创建而成。

## 静态代理
1. `Count.java`

    ```[java]
    package com.yangruihan.dao;

    /**
     * 定义一个账户接口
     */
    public interface Count {
        
        // 查看账户方法
        public void queryCount();

        // 修改账户方法
        public void updateCount();
    }
    ```

2. `CountImpl.java`

    ```[java]
    package com.yangruihan.dao.impl;

    import com.yangruihan.dao.Count;

    /**
     * 委托类（包含业务逻辑）
     */
    public class CountImpl implements Count {
        
        @Override
        public void queryCount() {
            System.out.println("查看用户...");
        }

        @Override
        public void updateCount() {
            System.out.println("修改账户...");
        }
    }
    ```

3. `CountProxy.java`

    ```[java]
    package com.yangruihan.dao.impl;

    import com.yangruihan.dao.Count;

    /**
     * 这是一个代理类（增强 CountImpl 实现类）
     */
    public class CountProxy implements Count {
        private CountImpl countImpl;

        /**
         * 覆盖默认构造器
         */
        public CountProxy(CountImpl countImpl) {
            this.CountImpl = countImpl;
        }

        @Override
        public void queryCount() {
            System.out.println("事务处理之前...");
            // 调用委托类的方法
            countImpl.queryCount();
            System.out.println("事务处理之后...");
        }

        @Override
        public void updateCount() {
            System.out.println("事务处理之前...");
            // 调用委托类的方法
            countImpl.updateCount();
            System.out.println("事务处理之后...");   
        }
    }
    ```

4. `TestCount.java`

    ```[java]
    package com.yangruihan.test;

    import com.yangruihan.dao.impl.CountImpl;
    import com.yangruihan.dao.impl.CountProxy;

    /**
     * 测试类
     */
    public class TestCount {
        public static void main(String[] args) {
            CountImpl countImpl = new CountImpl();
            CountProxy countProxy = new CountProxy(countImpl);
            countProxy.updateCount();
            countProxy.queryCount();
        }
    }
    ```

## 动态代理
观察上面代码可以发现每一个代理类只能为一个接口服务，这样一来程序开发中必然会产生过多的代理类，而且，所有的代理操作除了调用的方法不一样之外，其他的操作都一样，则此时肯定是有重复代码的。解决这一问题最好的做法是可以通过一个代理类完成全部的代理功能，那么此时就必须使用动态代理完成。

1. JDK 动态代理（包含一个类和一个接口）：

    - `InvocationHandler`接口：

        ```[java]
        public interface InvocationHandler {
            public Object invoke(Object proxy, Method method, Object[] args) throws Throwable;
        }
        ```

        其中参数说明：
        - Object proxy：指被代理的对象
        - Method method：要调用的方法
        - Object[] args：方法调用时所需要的参数

        可以将`InvocationHandler`接口的子类想象成一个代理的最终操作类，替换掉`ProxySubject`。

    - `Proxy`类：

        `Proxy`类是专门完成代理的操作类，可以通过此类为一个或多个接口动态地生成实现类，此类提供如下的操作方法：

        ```[java]
        public static Object newProxyInstance(ClassLoader loader, Class<?>[] interfaces, InvocationHandler h) throws IllegalArgumentException
        ```

        其中参数说明：
        - ClassLoader loader：类加载器
        - Class<?>[] interfaces：得到全部的接口
        - InvocationHandler h：得到`InvocationHandler`接口的子类实例

        *Ps：类加载器：*
        
            *在`Proxy`类中的`newProxyInstance()`方法中需要一个`ClassLoader`类的实例，`ClassLoader`实际上对应的是类加载器，在Java中主要有一下三种类加载器：*

            - *Booststrap ClassLoader：此加载器采用 C++ 编写，一般开发中是看不到的*
            - *Extendsion ClassLoader：用来进行扩展类的加载，一般对应的是`jre\lib\ext`目录中的类*
            - *AppClassLoader：（默认）加载`classpath`指定的类，是最常用的一种加载器*

    与静态代理类对照的是动态代理类，动态代理类的字节码在程序运行时由 Java 反射机制动态生成，无需程序员手工编写它的源代码。动态代理类不仅简化了编程工作，而且提高了软件系统的可扩展性，因为 Java 反射机制可以生成任意类型的动态代理类。`java.lang.reflect`包中的`Proxy`类和`InvocationHandler`接口提供了生成动态代理类的能力。

    **程序演示：**

    1. `BookFacade.java`

        ```[java]
        package com.yangruihan.dao;

        public interface BookFacade {
            public void addBook();
        }
        ```

    2. `BookFacadeImpl.java`

        ```[java]
        package com.yangruihan.dao.impl;

        import com.yangruihan.dao.BookFacade;

        public class BookFacadeImpl implements BookFacade {

            @Override
            public void addBook() {
                System.out.println("增加图书方法...");
            }
        }
        ```

    3. `BookFacadeProxy.java`

        ```[java]
        package com.yangruihan.proxy;

        import java.lang.reflect.InvocationHandler;
        import java.lang.reflect.Method;
        import java.lang.reflect.Proxy;

        /**
         * JDK 动态代理代理类
         */
        public class BookFacadeProxy implements InvocationHandler {
            private Object target;

            /**
             * 绑定委托对象并返回一个代理类
             */
            public Object bind(Object target) {
                this.target = target;

                // 取得代理对象
                return Proxy.newProxyInstance(target.getClass().getClassLoader(), target.getClass().getInterfaces(), this); // 该方法需要绑定接口（这是一个缺陷，cglib 弥补了这一缺陷）
            }

            /**
             * 调用方法
             */
            @Override
            public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                Object result = null;
                System.out.println("事务开始...");
                // 执行方法
                result = method.invoke(target, args);
                System.out.println("事务结束...");
                return result;
            }
        }
        ```

    4. `TestProxy.java`

        ```[java]
        package com.yangruihan.test;

        import com.yangruihan.dao.BookFacade;
        import com.yangruihan.dao.impl.BookFacadeImpl;
        import com.yangruihan.proxy.BookFacadeProxy;

        public class TestProxy {

            public static void main(String[] args) {
                BookFacadeProxy proxy = new BookFacadeProxy();
                BookFacade bookProxy = (BookFacade)proxy.bind(new BookFacadeImpl());
                bookProxy.addBook();
            }
        }
        ```

2. cglib 代理
    
    JDK 的动态代理机制只能代理实现了接口的类，而不能实现接口的类就不能实现 JDK 动态代理，cglib 是针对类来实现代理的，它的原理是对指定的目标类生成一个子类，并覆盖其中方法实现增强，但因为采用的是继承，所以不能对`final`修饰的类进行代理。

    **程序演示：**

    1. `BookFacade.java`

        ```[java]
        package com.yangruihan.dao;

        public interface BookFacade {
            public void addBook();
        }
        ```

    2. `BookFacadeImpl.java`

        ```[java]
        package com.yangruihan.dao.impl;

        /**
         * 这是没有实现接口的实现类
         */
        public class BookFacadeImpl {
            public void addBook() {
                System.out.println("增加图书的普通方法...");
            }
        }
        ```

    3. `BookFacadeProxy.java`

        ```[java]
        package com.yangruihan.proxy;

        import java.lang.reflect.Method;

        import net.sf.cglib.proxy.Enhancer;  
        import net.sf.cglib.proxy.MethodInterceptor;  
        import net.sf.cglib.proxy.MethodProxy;  

        /**
         * 使用 cglib 动态代理
         */
        public class BookFacadeCglib implements MethodInterceptor {
        
            private Object target;

            /**
             * 创建代理对象
             */
            public Object getInstance(Object target) {
                this.target = target;
                Enhancer enhancer = new Enhancer();
                enhancer.setSuperClass(this.target.getClass());

                // 回调方法
                enhancer.setCallBack(this);

                // 创建代理对象
                return enhancer.create();
            }

            /**
             * 回调方法
             */
            @Override
            public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable {
                System.out.println("事务开始...");
                proxy.invokeSuper(obj, args);
                System.out.println("事务结束...");

                return null;
            }
        }
        ```

    4. `TestCglib.java`

        ```[java]
        package com.yangruihan.test;

        import com.yangruihan.dao.BookFacadeImpl;
        import com.yangruihan.proxy.BookFacadeCglib;

        public class TestCglib {

            public static void main(String[] args) {
                BookFacadeCglib cglib = new BookFacadeCglib();
                BookFacadeImpl bookCglib = (BookFacadeImpl)cglib.getInstance(new BookFacadeImpl());
                
                bookCglib.addBook();
            }
        }
        ```
