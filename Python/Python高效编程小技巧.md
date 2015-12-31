# Python高效编程小技巧
参考资料：[Python 高效编程技能点](http://www.jianshu.com/p/6bb6e028e7eb)

## 1. 拆箱
变量声明利用拆箱这种方式，非常高效
```[python]
>>> a, b, c = 1, 2, 3
>>> a, b, c
(1, 2, 3)
>>> a, b, c = [1, 2, 3]
>>> a, b, c
(1, 2, 3)
>>> a, b, c = (1, 2, 3)
>>> a, b, c
(1, 2, 3)
>>> a
1
>>> a, b, c = [2 * i + 1 for i in range(3)]
>>> a, b, c
(1, 3, 5)
>>> a, b, c = (2 * i + 1 for i in range(3))
>>> a, b, c
(1, 3, 5)
>>> a, (b, c), d = [1, (2, 3), 4]
>>> a, b, c, d
(1, 2, 3, 4)
```

**拆箱的简单应用：变量交换**
```[python]
>>> a, b = 1, 2
>>> a, b = b, a
>>> a, b
(2, 1)
```

## 2. 列表操作
```[python]
>>> a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
>>> a[::2]
[0, 2, 4, 6, 8, 10]
>>> a[2:5:2]
[2, 4]

# 列表深拷贝
>>> a[::1]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# 逆序深拷贝
>>> a[::-1]
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

# 利用上面，可是很方便的实现列表倒置
>>> a = a[::-1]
>>> a
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

# 利用这一特性，很容易实现判断字符串是否回文
>>> a = list('123454321')
>>> result = True if a == a[::-1] else False
>>> result
True

# 给切割部分赋值，可以借此插入列表
>>> a = [1, 2, 3, 4, 5]
>>> a[2:3] = [0, 0]
>>> a
[1, 2, 0, 0, 4, 5]
>>> a[1:1] = [8, 9]
>>> a
[1, 8, 9, 2, 0, 0, 4, 5]

# 使用命令的方式进行列表切割
>>> a = [1, 2, 3, 4, 5]
>>> last_three = slice(-3, None)
>>> last_three
slict(-3, None, None)
>>> a[last_three]
[3, 4, 5]
```

## 3. 压缩器zip
### 3.1 列表 or 迭代器的压缩与解压缩
```[python]
>>> a = [1, 2, 3]
>>> b = ['a', 'b', 'c']
>>> z = zip(a, b)
>>> z
[(1, 'a'), (2, 'b'), (3, 'c')]
>>> zip(*z)
[(1, 2, 3), ('a', 'b', 'c')]
```

### 3.2 列表相邻元素压缩器
```[python]
>>> a = [1, 2, 3, 4, 5, 6]
>>> zip(*([iter(a)] * 2))
[(1, 2), (3, 4), (5, 6)]
>>> group_adjacent = lambda a, k: zip(*([iter(a)] * k))
>>> group_adjacent(a, 3)
[(1, 2, 3), (4, 5, 6)]
>>> zip(a[::2], a[1::2])
[(1, 2), (3, 4), (5, 6)]
>>> group_adjacent = lambda a, k: zip(*(a[i::k] for i in range(k)))
>>> group_adjacent(a, 3)
[(1, 2, 3), (4, 5, 6)]
```

### 3.3 用压缩器翻转字典
```[python]
>>> m = {'a': 1, 'b': 2, 'c': 3, 'd': 4}
>>> m.items()
[('a', 1), ('c', 3), ('b', 2), ('d', 4)]
>>> zip(m.values(), m.keys())
[(1, 'a'), (3, 'c'), (2, 'b'), (4, 'd')]
>>> mi = dict(zip(m.values(), m.keys()))
>>> mi
{1: 'a', 2: 'b', 3: 'c', 4: 'd'}
```

## 4. 列表展开
```[python]
>>> a = [[1, 2], [3, 4], [5, 6]]
>>> a
[[1, 2], [3, 4], [5, 6]]
>>> sum(a, [])
[1, 2, 3, 4, 5, 6]
>>> [x for l in a for x in l]
[1, 2, 3, 4, 5, 6]
```

## 5. 生成器表达式
```[python]
>>> a = (x ** 2 for x in xrange(10))
>>> next(a)
0
>>> next(a)
1
>>> next(a)
4
>>> next(a)
9
>>> sum(x ** 3 for x in xrange(10))
2025
```

## 6. 字典推导和集合推导
```[python]
# 最常见的推导
>>> list1 = [1, 2, 3, 4, 5]
>>> list2 = [x + 1 for x in list1]
>>> list2
[2, 3, 4, 5, 6]

# 求一个列表中的偶数元素
>>> some_list = [1, 2, 3, 8, 6, 4, 3, 4, 1]
>>> even_set = {x for x in some_list if x % 2 == 0}
>>> even_set
set([8, 2, 4, 6])

# 创造字典表
>>> d = {x: x % 2 == 0 for x in range(1, 11)}
>>> d
{1: False, 2: True, 3: False, 4: True, 5: False, 6: True, 7: False, 8: True, 9: False, 10: True}

# 翻转字典
>>> d = {'a': 1, 'b': 2, 'c': 3, 'd': 4}
>>> d
{'a': 1, 'c': 3, 'b': 2, 'd': 4}
>>> {v: k for k, v in d.items()}
{1: 'a', 2: 'b', 3: 'c', 4: 'd'}
```

## 7. Counter 计数器
```[python]
>>> from collections import Counter
>>> c = Counter('Hello World')
>>> c
Counter({'l': 3, 'o': 2, ' ': 1, 'e': 1, 'd': 1, 'H': 1, 'r': 1, 'W': 1})
>>> c.most_common(2)
[('l', 3), ('o', 2)]
```

## 8. 默认词典
一般情况下，空词典它就是空的，但是我们利用 collections 里的函数，可以实现默认的字典。
```[python]
>>> d = dict()
>>> d['a']
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'a'

# 你可以在括号里添加各种初始条件
>>> import collections
>>> m = collections.defaultdict(int)
>>> m['a']
0
>>> m = collections.defaultdict(lambda: '[default value]')
>> m['a']
'[default value]'
```

## 9. 利用 json 库打印出漂亮的 JSON 串
```[python]
>>> import json
>>> data = {"status": "OK", "count": 2, "results": [{"age": 27, "name": "Oz", "lactose_intolerant": True}, {"age": 29, "name": "Joe", "lactose_intolerant": False}]}
>>> print(json.dumps(data))
{"status": "OK", "count": 2, "results": [{"age": 27, "name": "Oz", "lactose_intolerant": true}, {"age": 29, "name": "Joe", "lactose_intolerant": false}]}

>>> print(json.dumps(data, indent=2))
{
  "status": "OK",
  "count": 2,
  "results": [
    {
      "age": 27,
      "name": "Oz",
      "lactose_intolerant": true
    },
    {
      "age": 29,
      "name": "Joe",
      "lactose_intolerant": false
    }
  ]
}
```

## 10. 列表中的几个最大、最小元素
```[python]
>>> import random, heapq
>>> a = [random.randint(0, 100) for __ in range(100)]
>>> smallest = heapq.nsmallest(5, a)
>>> largest = heapq.nlargest(5, a)
>>> smallest, largest
([0, 4, 4, 6, 7], [98, 98, 95, 94, 92])
>>> a
[73, 45, 80, 98, 33, 0, 66, 70, 78, 23, 59, 79, 24, 83, 29, 7, 64, 74, 32, 21, 94, 50, 12, 18, 28, 43, 80, 60, 42, 17, 54, 23, 41, 49, 13, 19, 69, 15, 53, 52, 4, 90, 6, 26, 92, 23, 79, 62, 26, 82, 76, 23, 46, 62, 55, 10, 19, 32, 89, 61, 73, 25, 64, 29, 92, 51, 50, 11, 71, 34, 29, 41, 84, 10, 92, 52, 64, 67, 51, 62, 20, 70, 91, 69, 74, 98, 26, 40, 4, 76, 46, 95, 32, 88, 91, 90, 81, 85, 40, 40]
```