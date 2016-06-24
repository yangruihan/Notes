# Pythonista 容易忽略的python编程方式
## Python 之禅
> The Zen of Python, by Tim Peters

> Beautiful is better than ugly.

> 优美胜于丑陋（Python以编写优美的代码为目标）

> Explicit is better than implicit.

> 明了胜于晦涩（优美的代码应当是明了的，命名规范，风格相似）

> Simple is better than complex.

> 简洁胜于复杂（优美的代码应当是简洁的，不要有复杂的内部实现）

> Complex is better than complicated.

> 复杂胜于凌乱（如果复杂不可避免，那代码间也不能有难懂的关系，要保持接口简洁）

> Flat is better than nested.

> 扁平胜于嵌套（优美的代码应当是扁平的，不能有太多的嵌套）

> Sparse is better than dense.

> 间隔胜于紧凑（优美的代码有适当的间隔，不要奢望一行代码解决问题）

> Readability counts.

> 可读性很重要（优美的代码是可读的）

> Special cases aren”t special enough to break the rules.

> Although practicality beats purity.

> 即便假借特例的实用性之名，也不可违背这些规则（这些规则至高无上）

> Errors should never pass silently.

> Unless explicitly silenced.

> 不要包容所有错误，除非你确定需要这样做（精准地捕获异常，不写except:pass风格的代码）

> In the face of ambiguity, refuse the temptation to guess.

> 当存在多种可能，不要尝试去猜测

> There should be one– and preferably only one –obvious way to do it.

> 而是尽量找一种，最好是唯一一种明显的解决方案（如果不确定，就用穷举法）

> Although that way may not be obvious at first unless you”re Dutch.

> 虽然这并不容易，因为你不是 Python 之父（这里的Dutch是指Guido）

> Now is better than never.

> Although never is often better than right now.

> 做也许好过不做，但不假思索就动手还不如不做（动手之前要细思量）

> If the implementation is hard to explain, it”s a bad idea.

> If the implementation is easy to explain, it may be a good idea.

> 如果你无法向人描述你的方案，那肯定不是一个好方案；反之亦然（方案测评标准）

> Namespaces are one honking great idea — let”s do more of those!

> 命名空间是一种绝妙的理念，我们应当多加利用（倡导与号召）

## python编程空格和缩进
1. 每次缩进使用4个空格

2. 不要使用Tab，更不要Tab和空格混用

3. 两个方法之间使用一个空行，两个Class之间使用两个空行

4. 添加一个空格在字典、列表、序列、参数列表中的“，“后，以及在字典中的”：“之后，而不是之前

5. 在赋值和比较两边放置一个空格（参数列表中除外）

6. 紧随括号后面或者参数列表前一个字符不要存在空格

## 使用如下方式交换pyhton的值
```python
b, a = a, b
 
# 其他例子
 
In [1]: people = ["David", "Pythonista", "15145551234"]
In [2]: name, title, phone = people
In [3]: name
Out[3]: "David"
In [4]: title
Out[4]: "Pythonista"
In [5]: phone
Out[5]: "15145551234"
 
这种语法在For循环中非常实用：
 
In [6]: people = [["David", "Pythonista", "15145551234"], ["Wu", "Student", "15101365547"]]
In [7]: for name, title, phone in people:
...: print name, phone
...:
David 15145551234
Wu 15101365547

PS：在使用这种语法时，需要确保左边的变量个数和右边tuple的个数一致，否则，Python会抛出ValueError异常。
```

## 合并字符串的值
```python
result = “,”.join(colors)
```

这样的效率要比使用for循环进行拼接的效率高，当list元素越多的时候，越明显

## 使用关键字`in`
当要判断一个key是否在字典中的时候

```python
d = {"a": 1, "b": 2}
if "c" in d:
print True
# DO NOT USE
if d.has_key("c"):
print True
for key in d:
print key
# DO NOT USE
for key in d.keys():
print key
```

Python的`dict`对象是对KEY做过`hash`的，而`keys()`方法会将`dict`中所有的KEY作为一个`list`对象；所以，直接使用in的时候执行效率会比较快，代码也更简洁

## 字典
`dict`是Python内置的数据结构，在写Python程序时会经常用到。这里介绍一下它的`get`方法和`defaultdict`方法

1. `get`

	在获取`dict`中的数据时，我们一般使用index的方式，但是如果KEY不存在的时候会抛出`KeyError`。这时候你可以使用`get`方法，使用方法：`dict.get(key, default=None)`，可以避免异常。例如：
	
	```python
	d = {"a": 1, "b": 2}
	print d.get("c") # None
	print d.get("c", 14) # 14
	```
	
2. `fromkeys`

	dict本身有个`fromkeys`方法，可以通过一个`list`生成一个`dict`，不过得提供默认的`value`，例如：
	
	```python
	# ?序列做 key,并提供默认value
	>>> dict.fromkeys(["a", "b", "c"], 1)
	# {"a": 1, "c": 1, "b": 1}请输入代码
	```

3. 有些情况下，我们需要给dict的KEY一个默认值，你可以这样写：

	```python
	equities = {}
	for (portfolio, equity) in data:
	equities.setdefault(portfolio, []).append(equity)
	```
	
	`setdefault`方法相当于"get, or set & get"，或者相当于"set if necessary, then get"
	
## defaultdict
`defaultdict()`和`namedtuple()`是`collections`模块里面2个很实用的扩展类型。一个继承自`dict`系统内置类型,一个继承自`tuple`系统内置类型

## 字典操作
在Python中，你可以使用`zip`方法将两个`list`组装成一个`dict`，其中一个`list`的值作为KEY，另外一个`list`的值作为VALUE：

```python
>>> given = ["John", "Eric", "Terry", "Michael"]
>>> family = ["Cleese", "Idle", "Gilliam", "Palin"]
>>> pythons = dict(zip(given, family))
>>> print pythons
{"John": "Cleese", "Michael": "Palin", "Eric": "Idle", "Terry": "Gilliam"}
```

相反的，你可以使用dict的`keys()`和`values()`方法来获取KEY和VALUE的列表：

```python
>>> pythons.keys()
["John", "Michael", "Eric", "Terry"]
>>> pythons.values()
["Cleese", "Palin", "Idle", "Gilliam"]
```

## python的`True`
在Python中，判断一个变量是否为`True`的时候，你可以这样做：

> False True

> False (== 0) True (== 1)

> "" (空字符串) 除 "" 之外的字符串("", "anything")

> 0, 0.0 除 0 之外的数字(1, 0.1, -1, 3.14)

> [], (), {}, set() 非空的list，tuple，set和dict ([0], (None,), [""])

> None 大部分的对象，除了明确指定为False的对象

对于自己声明的class，如果你想明确地指定它的实例是`True`或`False`，你可以自己实现class的`nonzero`或`len`方法。当你的class是一个`container`时，你可以实现`len`方法，如下：

```python
class MyContainer(object):
    def __init__(self, data):
        self.data = data
    def __len__(self):
    """ Return my length. """
        return len(self.data)
```

如果你的class不是`container`，你可以实现`nonzero`方法，如下：

```python
class MyClass(object):
    def __init__(self, value):
        self.value = value
    def __nonzero__(self):
    """ Return my truth value (True or False). """
        # This could be arbitrarily complex:
        return bool(self.value)
```

在Python 3.x中，`nonzero`方法被`bool`方法替代。考虑到兼容性，你可以在class定义中加上以下的代码：

```python
__bool__ = __nonzero__
```