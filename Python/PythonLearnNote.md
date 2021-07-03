# 第一章 初识 Python

*查看 Python 版本 `python --version`*

## 1.1 运行python代码的两种方式

1. repl 交互式环境
    
    read
    eval
    print
    loop

2. 运行文件
    python test.py

# 第二章 语言元素

## 2.1 数

### 2.1.1 整形 Integer

1、2、-1、-2、0 都是整数

整形对应的数据类型名称是`int`

*注：二进制、八进制、十六进制有不同的表示方法*

## 2.1.2 浮点数（小数） float number

1.1、2.1345、3.14

浮点数对应的数据类型名称是`float`

## 2.2 字符串

形如：`"Hello, World"`、`'Hello, World'`

字符串对应的类型`str`

字符串内部用到单双引号
1. `'I say: "hello"'`
2. `"I say: \"hello\""`

格式化字符串：

`f"{var1}"`

## 2.3 布尔

- `True` 表示真
- `False` 表示假

布尔对应的类型`bool`

只有这两种写法

## 2.4 命名

尽量用简单英语，可以长一点，但要明确含义

比如`dayOfWeek = 1`

*注：不用担心变量名太长，当你再次使用该变量的时候，编辑器会自动提示名字后续的字符，可以直接选择，不用全部手写*

变量名为**字母**、**数字**、**下划线**，其中**数字**不能开头

- _hello 行
- hello 行
- 1hello 不行

## 2.5 类型转换

使用 `type(xxx)`查看变量类型

- `int()`：将一个数值或字符串转换成整数，可以指定进制
- `float()`：将一个字符串转换成浮点数
- `str()`：将指定的对象转换成字符串形式，可以指定编码

# 第三章 分支结构

## 3.1 `if` 语句

```python
if xxx:
    doSomeWork()
# --- 以下可以没有 ---
elif xxx2: # else if
    doSomeWork2()
else:
    doOtherWork()
```

`if`可以嵌套，等价于

```python
if xxx:
    doSomeWork()
else:
    if xxx2: # else if
        doSomeWork2()
    else:
        doOtherWork()
```

布尔判断用`is`

```python
if b is True:
    pass
elif b is not True:
    pass
```

# 第四章 循环结构

## 4.1 `for`循环

形如：

```python
for x in xxx:
    doSomeWork()
```

`range`用法扩展：

- range(101)：可以用来产生0到100范围的整数，需要注意的是取不到101。
- range(1, 101)：可以用来产生1到100范围的整数，相当于前面是闭区间后面是开区间。
- range(1, 101, 2)：可以用来产生1到100的奇数，其中2是步长，即每次数值递增的值。
- range(100, 0, -2)：可以用来产生100到1的偶数，其中-2是步长，即每次数字递减的值。

## 4.2 `while`循环

```python
while condition:
    doSomeWork()
```

## 4.3 `break`打断循环

只能打断上一级的循环

```python
for i in range(5):
    
    for j in range(5):
        print(f'(x={i}, y={j})')

        if j == 1:
            break
```

# 快捷键

|快捷键|作用|
|:--:|:--:|
|`ctrl`+`/`|注释和取消注释|
|`ctrl`+`x`|剪切一行|
|`ctrl`+`shift`+`f`|格式化文件（美化文件格式）|
|选中内容+`tab`|向右移动选中内容|
|选中内容+`shift`+`tab`|向左移动选中内容|

# 英文对照表

|中文|英文|
|:---:|:---:|
|源代码|source code|
|语法|syntax|
|变量|variable|
|值|value|
|语句|statement|
|类型|type|
|函数|function|
|整形|integer (int)|
|浮点数（小数）|float|
|字符串|string (str)|
|布尔值|boolean (bool)|
|空值|None|
|列表|list|
|集合|set|
|字典|dictonary|
|元组|tuple|

*[参考链接](https://blog.csdn.net/baidu_33725271/article/details/70188334)*
