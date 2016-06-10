# 一些实用的python小建议

## 给dict设置默认值

这样能设置所有`key`的默认值为`[]`，包括新添的`key`

```python
from collections import defaultdict
context = defaultdict(list)
```

`setdefault`一次只能设置一个值，但好处是能使用链式语法，但`defaultdict`更快一些

```python
context = {}
context.setdefault('name_list', []).append('Fiona')
```

或者用`fromkeys`，用法`dict.fromkeys(seq[, value]))`，`value`默认是国际惯例的`None`

```python
name_list = ['kevin', 'robin']
context = {}.fromkeys(name_list, 9)
# {'kevin': 9, 'robin': 9}
 
context = dict.fromkeys([1, 2], True)  
# {1: True, 2: True}
```

## 列表去重的快速方法

比用 set 要快，来自：http://www.peterbe.com/plog/uniqifiers-benchmark

```python
{}.fromkeys(mylist).keys()
```

## 列表深复制
```python
a = [3, 2, 1]
b = a[:]
```

## 字典深复制
```python
a = {'male':0, 'female': 1}
b = a.copy()
```
