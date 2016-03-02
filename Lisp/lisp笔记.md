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

##5.函数
1. 函数一般使用**DEFUN**宏来定义。

    ```[lisp]
    (defun name (parameter*)
        "Optional documentation string."
        body-form*)
    ```
    
2. **任何符号**都可以用作函数名。通常函数名仅包含字典字符和连字符。

3. **可选参数**：

    ```[lisp]
    ;; 没有给出可选参数默认值的函数
    (defun foo (a b &optional c d) (list a b c d)) 
    
    ;; 给出可选参数默认值的函数
    (defun foo (a b &optional (c 10) (d 20)) (list a b c d))
    
    ;; 默认值表达式可以引用早先出现在形参列表中的形参
    ;; 例如：编写一个返回矩形的某种表示的函数，并且
    ;; 想要使它可以特别方便地产生正方形
    (defun make-rectangle (width &optional (height width)) ...)
    
    ;; 有时为了区分形参的值是默认值还是用户传入的值，
    ;; 并且为了避免用户传入与默认值相同的值，通过在
    ;; 形参标识符的默认值表达式之后添加另一个变量名
    (defun foo (a b &optional (c 3 c-supplied-p))
        (list a b c c-supplied-p))  ; 如果用户为c传值，则c-supplied-p为T
    ```
    
4. **剩余形参**：

    ```[lisp]
    ;; 如果函数带有&rest形参，那么任何满足了必要和可选
    ;; 形参之后的其余所有实参都将被收集到一个列表里成为
    ;; 该&rest形参的值
    (defun format (stream string &rest values) ...)
    (defun + (&rest numbers) ...)
    ```
    
5. **关键字形参**：

    ```[lisp]
    ;; 在任意必要的&optional和&rest形参之后，可以用&key
    ;; 以及任意数量的关键字形参标识符
    (defun foo (&key a b c) (list a b c))
    
    ; 调用
    (foo)                         ; (NIL NIL NIL)
    (foo :a 1)                  ; (1 NIL NIL)
    (foo :a 1 :c 2)           ; (1 NIL 2)
    
    ;; 如同可选形参一样，关键字形参也可以提供一个默认值，
    ;; 和一个-supplied-p变量名，默认值同样可以引用那些
    ;; 早先出现在形参列表中的形参
    (defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
        (list a b c b-supplied-p))
        
    ;; 如果希望调用者用来指定形参的关键字不同于实际形参
    ;; 名，可以用一个列表来替换该形参
    (defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
        (list a b c c-supplied-p))
        
    ; 调用
    (foo :apple 10 :box 20 :charlie 30)
    ```
    
6. 混合不同的形参类型时，声明的顺序：首先是必要形参，其次是可选形参，再次是剩余形参，最后才是关键字形参。
