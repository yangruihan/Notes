# Lua注意事项

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
