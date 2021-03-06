# Lua注意事项

## 实战注意事项

### table.insert 和 select 配合使用注意

```lua
for i = 1, select("#", ...) do
    table.insert(ret, select(i, ...))
end
```

抛出`bad argument #2 to 'insert' (number expected, got table)`异常

原因在于`select(i, ...)`会返回索引在i之后的全部参数，因此这里会将`table.insert`三个参数填满，导致报错

需要改成：

```lua
for i = 1, select("#", ...) do
    table.insert(ret, (select(i, ...)))
end
```

使用`(select(i, ...))`强制限定`select`返回一个参数即可

## 理论注意事项

- 在条件检测中Lua语言把**零**和**空字符串**也都视为真。

- 在少数情况下，当需要区分**整型值**和**浮点型值**时，可以使用函数`math.type`：

  ```lua
  math.type(3) --> integer
  math.type(3.0) --> float
  ```

- 通过与零进行按位或运算，可以把浮点型值强制转换为整型值

  ```lua
  2^53 --> 9.007199254741e+15
  2^53 | 0 --> 9007199254740992
  ```

- 函数`reverse`、`upper`、`lower`、`byte`和`char`不适用于UTF-8字符串，这是因为它们针对的都是一字节字符

- 唯一的例外就是，当函数只有一个参数且该参数是字符串常量或表构造器时，括号是可选的

  ```lua
  print "Hello World"    <-->  print("Hello World")
  dofile 'a.lua'         <-->  dofile('a.lua')
  print [[ a multi-line  <-->  print([[ a multi-line
   message ]]                   message ]])
  f{x=10, y=20}          <-->  f({x=10, y=20})
  type{}                 <-->  type({})
  ```
  
- 函数`io.write`可以读取任意数量的字符串（或者数字）并将其写入当前输出流。由于调用该函数时可以使用多个参数，因此应该避免使用`io.write(a..b..c)`，应该调用`io.write(a,b,c)`，后者可以用更少的资源达到同样的效果，并且可以避免更多的连接动作

- 调用函数`file:seek()`会返回当前的位置且不改变当前位置；调用函数`file:seek("set")`会将位置重置到文件开头并返回0；调用函数`file:seek("end")`会将当前位置重置到文件结尾并返回文件的大小

  ```lua
  function fsize(file)
    local current = file:seek()
    local size = file:seek("end")
    file:seek("set", current)
    return size
  end
  ```
  
- 函数`os.renam`e用于文件重命名，函数`os.remove`用于移除（删除）文件。需要注意的是，由于这两个函数处理的是真实文件而非流，所以它们位于os库而非io库中

- 和大多数其他编程语言不同，在Lua语言中，循环体内声明的局部变量的作用域包括测试条件

- 按照语法，return只能是代码块中的最后一句：换句话说，它只能是代码块的最后一句，或者是end、else和until之前的最后一句

- 在使用goto跳转时，Lua语言设置了一些限制条件。首先，标签遵循常见的可见性规则，因此不能直接跳转到一个代码块中的标签（因为代码块中的标签对外不可见）。其次，goto不能跳转到函数外（注意第一条规则已经排除了跳转进一个函数的可能性）。最后，goto不能跳转到局部变量的作用域

- 在模式匹配中，所有预置的字符分类及其对应的含义:

  |写法|含义|
  |:---|:---|
  |.|任意字符|
  |%a|字母|
  |%c|控制字符|
  |%d|数字|
  |%g|处空格外的可打印字符|
  |%l|小写字母|
  |%p|标点符号|
  |%s|空白字符|
  |%u|大写字母|
  |%w|字母和数字|
  |%x|十六进制数字|

  这些类的大写形式表示类的补集。例如，'%A'代表任意非字母的字符

- 在模式匹配中，魔法字符包括：

  `( ) . % + - * ? [ ] ^ $`

  百分号同样可以用于这些魔法字符的转义。因此，'%？'匹配一个问号，'%%'匹配一个百分号

- 函数`os.date`指示符

|||
|:---|:---|
|%a|星期几的简写（例如：Wed）|
|%A|星期几的全写（例如：Wednesday）|
|%b|月份的简写（例如：Sep）|
|%B|月份的全名（例如：September）|
|%c|日期和时间（例如：09/16/98 23:48:10）|
|%d|一个月中的第几天（16）[01-31]|
|%H|24小时制中的小时数（23）[00-23]|
|%I|12小时制中的小时数（11）[01-12]|
|%j|一年中的第几天（259）[001-365]|
|%m|月份（09）[01-12]|
|%M|分钟（48）[00-59]|
|%p|"am"或"pm"|
|%S|秒数（10）[00-60]|
|%w|星期（3）[0-6]|
|%W|一年中的第几周（37）[00-53]|
|%x|日期（09/16/98）|
|%X|时间（23:48:10）|
|%y|两位数的年份（98）[00-99]|
|%Y|完整的年份（1998）|
|%z|时区（例如，-0300）|
|%%|百分号|

- 在操作日期时，我们必须要小心。虽然归一化是以显而易见的方式进行的，但是也可能会有一些不明显的后果。例如，如果计算March 31之后的一个月，将会得到April 31，而实际上应该被归一化成May 1（April 30之后的一天）。尽管这听上去很自然，但实际上如果从结果（May 1）中减去一个月，得到的却是April 1而不是原来的March 31。请注意，这种不一致是日历机制导致的结果，与Lua语言无关

- 两个移位操作都会用0填充空出的位，这种行为通常被称为逻辑移位（logical shift）。Lua语言没有提供算术右移（arithmetic right shift），即使用符号位填充空出的位。我们可以通过向下取整除法（floor除法），除以合适的2的整数次幂来实现算术右移（例如，x//16与算术右移4位等价）

- 函数loadfile也是从文件中加载Lua代码段，但它不会运行代码，而只是编译代码，然后将编译后的代码段作为一个函数返回

- 可以通过error来抛出异常（throw an exception），然后用函数pcall来捕获（catch）异常，而错误信息则用来标识错误的类型

- 如果希望得到一个有意义的栈回溯，那么就必须在函数pcall返回前先将调用栈构造好。为了完成这个需求，Lua语言提供了函数xpcall。该函数与函数pcall类似，但它的第2个参数是一个消息处理函数（message handler function）。当发生错误时，Lua会在调用栈展开（stack unwind）前调用这个消息处理函数，以便消息处理函数能够使用调试库来获取有关错误的更多信息

- 函数require在表package.loaded中检査模块是否已被加载。如果模块已经被加载，函数require就返回相应的值

- 要强制函数require加载同一模块两次，可以先将模块从package.loaded中删除：下一次再加载这个模块时，函数require就会重新加载模块

- 请记住，模块在任何情况下只加载一次；至于如何处理冲突的加载，取决于模块自己

- 在使用一个环境变量的值时，Lua语言会将其中所有的";;"替换成默认路径。例如，如果将LUA_PATH_5_3设为"mydir/？.lua;;"，那么最终路径就会是模板"mydir/？.lua"后跟默认路径

- 函数pairs也有了对应的元方法，因此我们可以修改表被遍历的方式和为非表的对象增加遍历行为。当一个对象拥有__pairs元方法时，pairs会调用这个元方法来完成遍历

- 调用rawget（t,i）会对表t进行原始（raw）的访问，即在不考虑元表的情况下对表进行简单的访问。进行一次原始访问并不会加快代码的执行（一次函数调用的开销就会抹杀用户所做的这些努力）

- Lua语言中处理全局变量的方式：
  - 编译器在编译所有代码段前，在外层创建局部变量_ENV
  - 编译器将所有自由名称var变换为_ENV.var
  - 函数load（或函数loadfile）使用全局环境初始化代码段的第一个上值，即Lua语言内部维护的一个普通的表

- 通常，`_G`和`_ENV`指向的是同一个表

- **弱引用表（weak table）**、**析构器（finalizer）**和函数`collectgarbage`是在Lua语言中用来辅助垃圾收集器的主要机制

- 从程序员的角度看，字符串是值而不是对象。所以，字符串就像数值和布尔值一样，对于一个字符串类型的键来说，除非它对应的值被回收，否则是不会从弱引用表中被移除的

- 在Lua语言中，一个具有弱引用键和强引用值的表是一个瞬表

- 通过给对象设置一个具有非空`__gc`元方法的元表，就可以把一个对象标记为需要进行析构处理。如果不标记对象，那么对象就不会被析构

- 我们确实给对象o设置了元表，但是这个元表没有__gc元方法，因此对象没有被标记为需要进行析构处理。即使我们后续给元表增加了元方法`__gc`，Lua语言也发现不了这种赋值的特殊之处，因此不会把对象标记为需要进行析构处理

- 当垃圾收集器在同一个周期中析构多个对象时，它会按照对象被标记为需要析构处理的顺序逆序调用这些对象的析构器

- 在每个垃圾收集周期内，垃圾收集器会在调用析构器前清理弱引用表中的值，在调用析构器之后再清理键

- 每一个垃圾收集周期由四个阶段组成：标记（mark）、清理（cleaning）、清除（sweep）和析构（finalization)

- 标记阶段把根结点集合（root set）标记为活跃，根结点集合由Lua语言可以直接访问的对象组成。在Lua语言中，这个集合只包括C注册表

- 线程与协程的主要区別在于，一个多线程程序可以并行运行多个线程，而协程却需要彼此协作地运行，即在任意指定的时刻只能有一个协程运行，且只有当正在运行的协程显式地要求被挂起（suspend）时其执行才会暂停

- 当协程A唤醒协程B时，协程A既不是挂起状态（因为不能唤醒协程A），也不是运行状态（因为正在运行的协程是B）。所以，协程A此时的状态就被称为正常状态
