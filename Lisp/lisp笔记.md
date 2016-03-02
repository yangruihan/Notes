# LISP 笔记

## 1.Emacs快捷键
|快捷键|作用|
|:---:|:---:|
|C-h t|查看tutorial|
|C-h i|打开超文本浏览器Info|
|C-h ?|显示所有组合键列表|
|C-h k|显示输入任何组合键所对应的命令|
|C-x b|切换缓冲区（switch-to-buffer）|
|C-x C-f|创建一个新文件|
|C-c C-c|将当前定义发送给Lisp进行求值并编译（slime-compile-defun）|
|C-c C-z|返回REPL|
|C-x C-s|保存文件（save-buffer）|
|M-x slime|重启SLIME|
|C-c C-l|加载文件（slime-load-file）|
|C-c C-k|编译并加载当前缓冲区所关联的文件|

## 2.宏
|关键字|作用|举例|
|:---:|:---:|:---:|
|DEFUN|定义一个函数|(defun hello-world () (format t "hello, world!"))|
|DEFMACRO|定义一个宏|(defmacro name (expr) (...))|
|DEFVAR|定义一个变量|(defvar \*db\* null)|
|GETF|接受一个plist和一个符号<br>返回plist中跟在那个符号后面的值|(getf (list :a 1 :b 2) :a)|
|SETF|赋值操作符|(setf a 1)|
|PUSH|添加新项|(push cd \*db\*)|
|POP|弹出栈顶项|(pop *db*)|
|DOLIST|循环一个变量的所有元素<br>依次绑定每个元素到指定变量上<br>（这里是cd）|(dolist (cd \*db\*))|
|OR|短路“或”，类似“\|\|”|(or t 0)|
|AND|短路“与”，类似“&&”|(and t t)|
|LOOP|不断执行表达式体，直到调用RETURN|(loop (if (not (y-or-n-p "Yes? [y/n]:")) (return)))|
|WITH-OPEN-FILE|打开一个文件，将文件流绑定到一个变量上<br>执行一组表达式，最后关闭这个文件<br>它可以保证在表达式体求值出错时也能正确关闭文件|(with-open-file (out filename :direction :output :if-exists :supersede)<br>(...))|
|WITH-STANDARD-IO-SYNTAX|确保影响输出行为的特定变量可以被设置成它们的标准值|(with-standard-io-syntax (print \*db\* out))|
|IF|判断语句|(if test-form then-form [else-form])|

## 3.内置函数
|内置函数名|作用|举例|
|:---:|:---:|:---:|
|FORMAT|接受一个值，并输出到指定位置|(format t "Hello, world!")|
|LIST|定义一个列表|(list 1 2 3) 或 (list :a 1 :b 2 :c 3)|
|FORCE-OUTPUT|将某一输出流信息强制输出<br>而不等待换行符出现|(force-output t)|
|READ-LINE|读取单行文本|(read)|
|PARSE-INTEGER|将字符串转换成Integer类型|(parse-integer "123")|
|Y-OR-N-P|输入了没有以y、Y、n或N开始的内容时<br>重新提示输入|(y-or-n-p "Yes? [y/n]: ")|
|REMOVE-IF-NOT|接受一个谓词和一个原始列表<br>然后返回一个仅包含原始列表中<br>匹配该谓词的所有元素的新列表|(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))|
|EVENP|当参数是偶数时返回真|(evenp 2)|
|EQUAL|判断两个字符串参数是否相等|(equal "1" "2")|
|MAPCAR|映射在一个列表上，然后返回一个新的列表<br>其中含有在原来列表的每个元素上<br>调用一个函数所得到的结果|(mapcar #'(lambda (a) (+ 1 a)) '(1 2 3))|
|REVERSE|接受一个列表作为参数<br>并返回一个逆序的新列表|(reverse '(1 2 3))|

##4.语法和语义
1. 数字的表示方法很简单：任何数位的序列将被读取为一个数字，它们可能有一个**前缀标识（＋或－）**，还可能会有一个**十进制点（.）**或者**斜杠（/）**，或是以一个**指数标记**结尾。

    ```[lisp]
    123                 ;整数一百二十三
    3/7                  ;比值七分之三
    1.0                   ; 默认精度的浮点数一
    1.0e0               ; 同一个浮点数的另一种写法
    1.0d0               ; 双精度浮点数一
    +42                  ; 整数四十二
    -42                   ; 整数负四十二
    -1/4                 ; 比值负四分之一
    -2/8                 ; 负四分之一的另一种写法
    246/2              ; 整数一百二十三的另一种写法 
    ```

2. 字符串是由双引号所包围着的。在字符串中，一个**反斜杠（/）**会转义接下来的任意字符。两个在字符串中**必须**被转义的字符是***双引号（"）***和***反斜杠（/）***

3. 几乎任何字符都可以出现在一个**名字**里，出**空白字符**以外。且有十个字符被用于其他句法目的而不能出现在名字里，它们是：**开括号和闭括号、双引号和单引号、反引号、逗号、冒号、分号、反斜杠以及竖线。**。语言标准所定义的名字只使用字母表字符（A-Z）外加\*、+、-、/、1、2、<、=、>以及&。

4. 符号**NIL**是唯一的假值，其他所有的都是真值。符号**T**是**标准**的真值。
