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

## 时间转换相关
### 获取今天的年月日时间(date)
```python
from datetime import datetime
 
n_date = datetime.now().date()
n_date = datetime.today().date()
```

### date -> datetime
```python
from datetime import datetime
 
b = datetime.combine(n_date, datetime.min.time())
# datetime.datetime(2015, 9, 8, 0, 0)
```

### datetime -> date
```python
# datetime.datetime(2015, 6, 5, 11, 45, 45, 393548)
a = datetime.datetime()
# datetime.datetime(2016, 6, 5)
b = a.date()
```

### time.struct_time -> datetime
一般`time.localtime()`或者用`time.striptime()`得到的就是`time.struct_time`

使用位置参数

```python
structTime = time.localtime()
datetime.datetime(*structTime[:6])
# datetime.datetime(2009, 11, 8, 20, 32, 35)
```

或者使用`datetime.fromtimestamp`，但是要注意此处的时间不能早于 1970-01-01 00:00

```python
from time import mktime
from datetime import datetime
 
dt = datetime.fromtimestamp(mktime(struct))
```

### 计算日期之差
```python
from datetime import date
 
d0 = date(2008, 8, 18)
d1 = date(2008, 9, 26)
delta = d0 - d1
print delta.days
```

### 获取milliseconds(13位数字)
```python
import time
from datetime import datetime
 
time.time()  # 1441769033.549239
int(time.time() * 1000)   # 1441769033549
 
# or
def unix_time_milliseconds:
    time_gap = datetime.utcnow() - datetime.utcfromtimestamp(0)
    return int(time_gap.total_seconds() * 1000)   # 1441769033549
```

## 使用`map`作 iterator
当将`func`设置为`None`时，可起到 iterator 的作用，结果类似于使用了`zip`，唯一的区别是`map`可以按最长的列表扩展

python2.x 中的`itertools.zip_longest`可以起到与此处`map`相同的效果

```python
map(None, xrange(3), xrange(10,12))
# [(0, 10), (1, 11), (2, None)]
zip(xrange(3), xrange(10,12))
# [(0, 10), (1, 11)]
```

## 判断奇数
自然是使用位操作最快了

```python
if a & 1:
    print 'it is even'
```

## dict删除key
要删除的`key`数量较多(超多一半)的话，建议重新生成`dict`；如果数量较少，在`pop`和`del`都可以的情况下，`del`稍快一些

```python
python -m timeit -s "d = {'f':1,'foo':2,'bar':3}" "d1 = d.copy()" "for k in d1.keys():" "  if k.startswith('f'):" "    del d1[k]"
# 1000000 loops, best of 3: 0.827 usec per loop
```

```python
python -m timeit -s "d = {'f':1,'foo':2,'bar':3}" "d1 = d.copy()" "for k in d1.keys():" "  if k.startswith('f'):" "    d1.pop(k)"
# 1000000 loops, best of 3: 0.96 usec per loop
```