# C 语言中的 `setjmp` 和 `longjmp`（转）

原文：https://www.cnblogs.com/hazir/p/c_setjmp_longjmp.html

在 C 语言中，我们不能使用 `goto` 语句来跳转到另一个函数中的某个 `label` 处；但提供了两个函数——`setjmp` 和 `longjmp`来完成这种类型的分支跳转。后面我们会看到这两个函数在处理异常上面的非常有用

## `setjmp` 和 `longjmp` 使用方法

我们都知道要想在一个函数内进行跳转，可以使用 `goto` 语句（不知怎么该语句在中国学生眼中就是臭名昭著，几乎所有国内教材都一刀切地教大家尽量不要使用它，但在我看来，这根本不是语言的问题，而是使用该语言的人，看看 Linux 内核中遍地是 `goto` 语句的应用吧！），但如果从一个函数内跳转到另一个函数的某处，`goto` 是不能完成的，那该如何实现呢？

### 函数间跳转原理

我们要实现的一个 `GOTO` 语句（我自己定义的），能实现在函数间进行任意跳转，如下例，在函数 `g()` 中有条语句`GOTO Label;` 可以跳转到 `f()` 函数的 `Label:` 标签所指向的位置，那么我们该如何实现呢？

```c
void f()
{
    //...
    Label:
    //...
}

void g()
{
    //...
    GOTO Label;
    //...
}
```

首先我们要知道，实现这种类型的跳转，和操作系统中任务切换的上下文切换有点类似，我们只需要恢复 `Label` 标签处函数上下文即可。函数的上下文包括以下内容：

- 函数栈帧，主要是栈帧指针BP和栈顶指针SP

- 程序指针PC，此处为指向 Label 语句的地址

- 其它寄存器，这是和体系相关的，在 x86 体系下需要保存有的 AX/BX/CX 等等 callee-regs。

这样，在执行 `GOTO Label;` 这条语句，我们恢复 `Label` 处的上下文，即完成跳转到 `Label` 处的功能。

如果你读过 Linux 操作系统进程切换的源码，你会很明白 Linux 会把进程的上下文保存在 `task_struct` 结构体中，切换时直接恢复。这里我们也可以这样做，将 `Label` 处的函数上下文保存在某个结构体中，但执行到 `GOTO Label` 语句时，我们从该结构体中恢复函数的上下文。

这就是函数间进行跳转的基本原理，而 C 语言中 `setjmp` 和 `longjmp` 就为我们完成了这样的保存上下文和切换上下文的工作。

### 函数原型

```c
#include <setjmp.h>
int setjmp(jmp_buf env);
```

`setjmp` 函数的功能是将函数在此处的上下文保存在 `jmp_buf` 结构体中，以供 `longjmp` 从此结构体中恢复。

- 参数 `env` 即为保存上下文的 `jmp_buf` 结构体变量；

- 如果直接调用该函数，返回值为 0； 若该函数从 `longjmp` 调用返回，返回值为非零，由 `longjmp` 函数提供。根据函数的返回值，我们就可以知道 `setjmp` 函数调用是第一次直接调用，还是由其它地方跳转过来的。


```c
void longjmp(jmp_buf env, int val);
```

`longjmp` 函数的功能是从 `jmp_buf` 结构体中恢复由 `setjmp` 函数保存的上下文，该函数不返回，而是从 `setjm`p 函数中返回。

- 参数 `env` 是由 `setjmp` 函数保存过的上下文。

- 参数 `val` 表示从 `longjmp` 函数传递给 `setjmp` 函数的返回值，如果 `val` 值为0， `setjmp` 将会返回1，否则返回 `val`。

- `longjmp` 不直接返回，而是从 `setjmp` 函数中返回，`longjmp` 执行完之后，程序就像刚从 `setjmp ` 函数返回一样。

### 简单实例

下面是个简单的例子，虽然还只是函数内跳转，但足以说明这两个函数的功能了。

![](https://f.cloud.github.com/assets/3265880/1460659/19d4db44-445a-11e3-8468-aab15a080f57.png)


运行该程序得到的结果为：

```
i = 0
i = 2
```

## C 语言异常处理

Java、C# 等面向对象语言中都有异常处理的机制，如下就是典型的 Java 中异常处理的代码，两个数相除，如果被除数为0抛出异常，在函数 f() 中可以获取该异常并进行处理：

```java
double divide(double to, double by) throws Bad {
    if(by == 0)
        throw new Bad ("Cannot / 0");
    return to / by;
}

void f() {
    try {
        divide(2, 0);
        //...   
    } catch (Bad e) {
        print(e.getMessage());
    }
    print("done");
}
```

在 C 语言中虽然没有类似的异常处理机制，但是我们可以使用 `setjmp` 和 `longjmp` 来模拟实现该功能，这也是这两个函数的一个重要的应用：

```c
static jmp_buf env;

double divide(double to, double by)
{
    if(by == 0)
        longjmp(env, 1);
    return to / by;
}

void f() 
{
    if (setjmp(env) == 0)
        divide(2, 0);
    else
        printf("Cannot / 0");
    printf("done");
}
```

如果复杂一点，可以根据 `longjmp` 传递的返回值来判断各种不同的异常，来进行区别的处理，代码结构如下：

```c
switch(setjmp(env)):
    case 0:         //default
        //...
    case 1:         //exception 1
        //...
    case 2:         //exception 2
        //...
    //...
```

关于使用 C 语言来处理异常，可以参见这篇[文章](http://www.di.unipi.it/~nids/docs/longjump_try_trow_catch.html)，介绍了更多复杂的结构，但无外乎就是 `setjmp` 和 `longjmp` 的应用。

## 参考资料
- http://en.wikipedia.org/wiki/Longjmp

- www.cs.purdue.edu/homes/cs240/lectures/Lecture-19.pdf