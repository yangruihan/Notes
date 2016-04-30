# Python的函数式编程–从入门到⎡放弃⎦
## 前言
很早以前就听说过了函数式编程，印象中是一种很晦涩难懂的编程模式，但却一直没有去进行了解。

恰好这周组内的周会轮到我主持，一时也没想到要分享什么。灵光一闪，就选定函数式编程这个主题吧，反正组里的同事都没有学过，只需要讲解入门方面的知识就好，也正好可以借这个机会逼迫自己去学习下这种新的编程方式。

经过初步了解，发现支持函数式编程的语言挺多的，除了像 Lisp、Scheme、Haskell、Erlang 这样专用的函数式编程语言，我们常用的好多通用型编程语言（如Java、Python、Ruby、Javascript等）都支持函数式编程模式。考虑了下实际情况，最终还是选择 Python 作为函数式编程的入门语言，因为组内同事都熟悉Python，以此作为切入点不会产生太大困难。

经过查询资料和初步学习，对函数式编程有了些概念，经过整理，便形成了分享PPT。

以下便是这次分享的内容。

## 目标
通常，我们在新学习一门技术或者编程语言的时候，通常都会先从相关概念和特性入手。对于新接触函数式编程的人来说，可能会想知道如下几点：

- 什么是函数式编程？

- 函数式编程的特点？

- 函数式编程的用途？

- 函数式编程相比于命令式编程和面向对象编程的优缺点？

但是我这次分享却没有按照这个思路，因为我感觉在一开始就向听众灌输太多概念性的东西，反倒会让听众感到迷糊。因为经过查询资料发现，对于什么是函数化编程，很难能有一个协调一致的定义。而且由于我也是新接触，自身的理解可能会存在较大的偏差。

因此，我决定分享内容尽量从大家熟悉的命令式编程切入，通过大量实例来向听众展现函数式编程思维方式的不同之处。在这之后，再回过头看这几个问题，相信听众应该都会有更深刻的理解。

考虑到实际情况，本次分享希望能达成的目标是：

- 了解函数式编程与命令式编程的主要区别

- 掌握 Python 语言函数式编程的基本函数和算子

- 会将简单的命令式编程语句转换为函数式编程

## 命令式编程 & 函数式编程
首先从大家熟悉的命令式编程开始，我们先回顾下平时在写代码时主要的情景。

其实，不管我们的业务代码有多复杂，都离不开以下几类操作：

- 函数定义：def

- 条件控制：if, elif, else

- 循环控制：for, break, continue, while

当然，这只是部分操作类型，除此之外还应该有类和模块、异常处理等等。但考虑到是入门，我们就先只关注上面这三种最常见的操作。

对应地，函数式编程也有自己的关键字。在 Python 语言中，用于函数式编程的主要由**3个基本函数**和**1个算子**。

- 基本函数：map()、reduce()、filter()

- 算子(operator)：lambda

令人惊讶的是，仅仅采用这几个函数和算子就基本上可以实现任意 Python 程序。

当然，能实现是一回事儿，实际编码时是否这么写又是另外一回事儿。估计要真只采用这几个基本单元来写所有代码的话，不管是在表达上还是在阅读上应该都挺别扭的。不过，尝试采用这几个基本单元来替代上述的函数定义、条件控制、循环控制等操作，对理解函数式编程如何通过函数和递归表达流程控制应该会很有帮助。

在开始尝试将命令式编程转换为函数式编程之前，我们还是需要先熟悉下这几个基本单元。

## Python函数式编程的基本单元
### lambda
`lambda`这个关键词在很多语言中都存在。简单地说，它可以实现函数创建的功能。

如下便是`lambda`的两种使用方式。

```python
func1 = lambda : <expression()>
func2 = lambda x : <expression(x)>
func3 = lambda x,y : <expression(x,y)>
```
在第一条语句中，采用`lambda`创建了一个无参的函数`func1`。这和下面采用def创建函数的效果是相同的。

```python
def func1():
    <expression()>
```

在第二条和第三条语句中，分别采用`lambda`创建了需要传入 1 个参数的函数`func2`，以及传入 2 个参数的函数`func3`。这和下面采用def创建函数的效果是相同的。

```python
def func2(x):
    <expression(x)>

def func3(x,y):
    <expression(x,y)>
```

需要注意的是，调用`func1`的时候，虽然不需要传入参数，但是必须要带有括号()，否则返回的只是函数的定义，而非函数执行的结果。这个和在`ruby`中调用无参函数时有所不同，希望`ruby`程序员引起注意。

```python
>>> func = lambda : 123
>>> func
<function <lambda> at 0x100f4e1b8>
>>> func()
123
```

另外，虽然在上面例子中都将`lambda`创建的函数赋值给了一个函数名，但这并不是必须的。从下面的例子中大家可以看到，很多时候我们都是直接调用`lambda`创建的函数，而并没有命名一个函数，这也是我们常听说的匿名函数的由来。

### map()
`map()`函数的常见调用形式如下所示：

```python
map(func, iterable)
```

`map()`需要两个必填参数，第一个参数是一个**函数名**，第二个参数是**一个可迭代的对象**，如列表、元组等。

`map()`实现的功能很简单，就是将第二个参数（iterable）中的每一个元素分别传给第一个参数（func），依次执行函数得到结果，并将结果组成一个新的`list`对象后进行返回。返回结果永远都是一个`list`。

简单示例如下：

```python
>>> double_func = lambda s : s * 2
>>> map(double_func, [1,2,3,4,5])
[2, 4, 6, 8, 10]
```

除了传入一个可迭代对象这种常见的模式外，`map()`还支持传入多个可迭代对象。

```python
map(func, iterable1, iterable2)
```

在传入多个可迭代对象的情况下，`map()`会依次从所有可迭代对象中依次取一个元素，组成一个元组列表，然后将元组依次传给`func`；若可迭代对象的长度不一致，则会以`None`进行补上。

通过以下示例应该就比较容易理解。

```python
>>> plus = lambda x,y : (x or 0) + (y or 0)
>>> map(plus, [1,2,3], [4,5,6])
[5, 7, 9]
>>> map(plus, [1,2,3,4], [4,5,6])
[5, 7, 9, 4]
>>> map(plus, [1,2,3], [4,5,6,7])
[5, 7, 9, 7]
```

在上面的例子中，之所以采用`x or 0`的形式，是为了防止`None + int`出现异常。

需要注意的是，可迭代对象的个数应该与`func`的参数个数一致，否则就会出现异常，因为传参个数与函数参数个数不一致了，这个应该比较好理解。

```python
>>> plus = lambda x,y : x + y
>>> map(plus, [1,2,3])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: <lambda>() takes exactly 2 arguments (1 given)
```

另外，`map()`还存在一种特殊情况，就是`func`为`None`。这个时候，`map()`仍然是从所有可迭代对象中依次取一个元素，组成一个元组列表，然后将这个元组列表作为结果进行返回。

```python
>>> map(None, [1,2,3,4])
[1, 2, 3, 4]
>>> map(None, [1,2,3,4], [5,6,7,8])
[(1, 5), (2, 6), (3, 7), (4, 8)]
>>> map(None, [1,2,3,4], [5,6,7])
[(1, 5), (2, 6), (3, 7), (4, None)]
>>> map(None, [1,2,3,4], [6,7,8,9], [11,12])
[(1, 6, 11), (2, 7, 12), (3, 8, None), (4, 9, None)]
```

### reduce()
`reduce()`函数的调用形式如下所示：

```python
reduce(func, iterable[, initializer])
```

`reduce()`函数的功能是对可迭代对象（iterable）中的元素从左到右进行累计运算，最终得到一个数值。第三个参数initializer是初始数值，可以空置，空置为None时就从可迭代对象（iterable）的第二个元素开始，并将第一个元素作为之前的结果。

文字描述可能不大清楚，看下`reduce()`的源码应该就比较清晰了。

```python
def reduce(function, iterable, initializer=None):
    it = iter(iterable)
    if initializer is None:
        try:
            initializer = next(it)
        except StopIteration:
            raise TypeError('reduce() of empty sequence with no initial value')
    accum_value = initializer
    for x in it:
        accum_value = function(accum_value, x)
    return accum_value
```

再加上如下示例，对`reduce()`的功能应该就能掌握了。

```python
>>> plus = lambda x, y : x + y
>>> reduce(plus, [1,2,3,4,5])
15
>>> reduce(plus, [1,2,3,4,5], 10)
25
```

### filter()
`filter()`函数的调用形式如下：

```python
filter(func, iterable)
```

`filter()`有且仅有两个参数，第一个参数是一个函数名，第二个参数是一个可迭代的对象，如列表、元组等。

`filter()`函数的调用形式与`map()`比较相近，都是将第二个参数（iterable）中的每一个元素分别传给第一个参数（func），依次执行函数得到结果；差异在于，`filter()`会判断每次执行结果的bool值，并只将bool值为`True`的筛选出来，组成一个新的列表并进行返回。

```python
>>> mode2 = lambda x : x % 2
>>> filter(mode2, [1,2,3,4,5,6,7,8,9,10])
[1, 3, 5, 7, 9]
```

以上便是Python函数式编程基本单元的核心内容。

接下来，我们就开始尝试采用新学习到的基本单元对命令式编程中的条件控制和循环控制进行转换。

## 替换条件控制语句
在对条件控制进行替换之前，我们先来回顾下 Python 中对布尔表达式求值时进行的“短路”处理。

什么叫“短路”处理？简单地讲，就是如下两点：

- 在`f(x) and g(y)`中，当`f(x)`为`False`时，不会再执行`g(y)`，直接返回`False`

- 在`f(x) or g(y)`中，当`f(x)`为`True`时，不会再执行`g(y)`，直接返回`True`

结论是显然易现的，就不再过多解释。

那么，对应到条件控制语句，我们不难理解，如下条件控制语句和表达式是等价的。

```python
# flow control statement
if <cond1>:   func1()
elif <cond2>: func2()
else:         func3()
```

```python
# Equivalent "short circuit" expression
(<cond1> and func1()) or (<cond2> and func2()) or (func3())
```

通过这个等价替换，我们就去除掉了`if/elif/else`关键词，将条件控制语句转换为一个表达式。那这个表达式和函数式编程有什么关系呢？

这时我们回顾上面讲过的`lambda`，会发现`lambda`算子返回的就是一个表达式。

基于这一点，我们就可以采用`lambda`创建如下函数。

```python
>>> pr = lambda s:s
>>> print_num = lambda x: (x==1 and pr("one"))
....                  or (x==2 and pr("two"))
....                  or (pr("other"))
>>> print_num(1)
'one'
>>> print_num(2)
'two'
>>> print_num(3)
'other'
```

通过函数调用的结果可以看到，以上函数实现的功能与之前的条件控制语句实现的功能完全相同。

到这里，我们就实现了命令式条件控制语句向函数式语句的转换。并且这个转换的方法是通用的，所有条件控制语句都可以采用这种方式转换为函数式语句。

## 替换循环控制语句
接下来我们再看循环控制语句的转换。在Python中，循环控制是通过for和while这两种方式实现的。

### 替换 for 循环
`for`循环语句的替换十分简单，采用`map()`函数就能轻松实现。这主要是因为`for`语句和`map()`原理相同，都是对可迭代对象里面的每一个元素进行操作，因此转换过程比较自然。

```python
# statement-based for loop
for e in lst:  func(e)

# Equivalent map()-based loop
map(func, lst)
```

```python
>>> square = lambda x : x * x
>>> for x in [1,2,3,4,5]: square(x)
...
1
4
9
16
25
>>> map(square, [1,2,3,4,5])
[1, 4, 9, 16, 25]
```

### 替换while循环
`while`循环语句的替换相比而言就复杂了许多。

下面分别是`while`循环语句及其对应的函数式风格的代码。

```python
# statement-based while loop
while <condition>:
    <pre-suite>
    if <break_condition>:
        break
    else:
        <suite>

# Equivalent FP-style recursive while loop
def while_block():
    <pre-suite>
    if <break_condition>:
        return 1
    else:
        <suite>
    return 0

while_FP = lambda: <condition> and (while_block() or while_FP())
while_FP()
```

这里的难点在于，函数式`while_FP`循环采用了递归的概念。当为`True`时，进入循环体，执行`while_block()`；若为`True`时，返回 1，`while_FP()`调用结束；若为`False`时，返回 0，会继续执行`or`右侧的`while_FP()`，从而实现递归调用；若始终为`False`，则会持续递归调用`while_FP()`，这就实现了`while`语句中同样的功能。

为了对函数式的`while`循环有更深刻的理解，可以再看下如下示例。这个例子是在网上找的，实现的是`echo`功能：输入任意非“quit”字符时，打印输入的字符；输入“quit”字符时，退出程序。

```python
➜  PythonFP python pyecho.py
IMP -- 1
1
IMP -- 2
2
IMP -- abc
abc
IMP -- 1 + 1
1 + 1
IMP -- quit
quit
➜  PythonFP
```

如下便是分别采用过程式和函数式语句实现的“echo”功能。

```python
# imperative version of "echo()"
def echo_IMP():
    while 1:
        x = raw_input("IMP -- ")
        print x
        if x == 'quit':
            break

echo_IMP()
```

```python
def monadic_print(x):
    print x
    return x

# FP version of "echo()"
echo_FP = lambda: monadic_print(raw_input("FP -- "))=='quit' or echo_FP()
echo_FP()
```

## 更多示例
到此为止，我们对函数式编程总算有了点认识，到达之前设定的目标应该是没有问题了，看来函数式编程也并没有想象中的那么难懂。

然而，这都只是函数式编程的皮毛而已，不信？再看下如下示例。

这个示例也是在网上找的，实现的是两个列表笛卡尔积的筛选功能，找出笛卡尔积元组集合中两个元素之积大于25的所有元组。

```python
bigmuls = lambda xs,ys: filter(lambda (x,y):x*y > 25, combine(xs,ys))
combine = lambda xs,ys: map(None, xs*len(ys), dupelms(ys,len(xs)))
dupelms = lambda lst,n: reduce(lambda s,t:s+t, map(lambda l,n=n: [l]*n, lst))

print bigmuls([1,2,3,4],[10,15,3,22])

[(3, 10), (4, 10), (2, 15), (3, 15), (4, 15), (2, 22), (3, 22), (4, 22)]
```

虽然这个例子中`lambda/map/reduce/filter`都是我们已经比较熟悉了的基本单元，但是经过组合后，理解起来还是会比较吃力。

## 总结
看到这里，有的同学就开玩笑说我这标题名称非常贴切，《Python的函数式编程–从入门到⎡放弃⎦》，因为以后在工作中应该也不会再尝试使用函数式编程了，^_^。

不过，我还是觉得函数式编程挺有意思的，更高级的特性后面值得再继续学习。即使代码不用写成pure函数式风格，但在某些时候局部使用`lambda/map/reduce/filter`也能大大简化代码，也是一个不错的选择。

另外，通过此次分享，再次切身体会到了教授是最好的学习方式，只有当你真正能将一个概念讲解清楚的时候，你才算是掌握了这个概念。

## 参考链接
http://www.ibm.com/developerworks/linux/library/l-prog/index.html