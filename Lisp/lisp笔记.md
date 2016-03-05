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
|DEFVAR|定义一个全局变量|(defvar \*db\* nil)|
|DEFPARAMETER|定义一个全局变量|(defparameter \*i\* 0)|
|DEFCONSTANT|定义一个全局常量|(defconstant +width+ 100)|
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
|INCF|以默认为1对一个位置的值进行递增|(incf x)|
|DECF|以默认为1对一个位置的值进行递减|(decf x)|
|ROTATEF|在位置之间轮换它们的值|(rotatef a b)|
|SHIFTF|左移位置上的各个值|(shiftf a b 10)|

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
|RETURN-FROM|立即以任何值从函数中间返回|(return-from fname value)|
|FUNCALL|通过函数对象调用函数，期待单独的参数|(funcall #'+ 1 2)|
|APPLY|通过函数对象调用函数，期待一个列表|(apply #'+ '(1 2))|
|EXP|返回以e为底以实参为指数的值|(exp 1)|
|LET|引入新变量|(let (variable\*) body-form\*)|
|LET\*|引入新变量，且每个变量的初始值<br>形式可以引用早先引入的变量|(let\* ((x 10) (y (+ x 10))) (list x y))|
|get-universal-time|返回当前时间的毫秒形式|(get-universal-time)|
|sleep|是线程休眠，以秒为单位|(sleep 60)|
|isqrt|求根号|(isqrt 4)|
|zerop|判断参数是否为0|(zerop 0)|
|mod|求余|(mod 3 2)|
|MACROEXPAND-1|接受任何Lisp表达式作为参数<br>返回做宏展开一层的结果|(macroexpand-1 '(...))|
|GENSYM|每次被调用时返回唯一的符号|(gensym)|

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

7. 使用`RETURN-FROM`能立即以任何值从函数中间返回。

8. **LAMBDA**表达式形式如下：

    ```[lisp]
    (lambda (parameters) body)
    ```

##6.变量
1. Common Lisp提供了两种创建全局变量的方式：**DEFVAR**和**DEFPARAMETER**。两种形式都接受一个变量名、一个初始值以及一个可选的文档字符串。

2. 从实践上来讲，应该使用**DEFVAR**来定义某些变量，这些变量所含数据是应持久存在的，即使用到该变量的源码发生改变时也应如此。

3. **常值变量（constant variable）**又叫**常量**。所有的常量都是**全局**的，并且使用**DEFCONSTANT**定义：

    ```[lisp]
    (defconstant name initial-value-form [documentation-string])
    ```

4. **赋值**使用**SETF**宏：

    ```[lisp]
    (setf place value)
    ```

##7.宏
1. **IF**宏，条件判断语句：

    ```[lisp]
    (if condition then-form [else-form])
    ```

2. **PROGN**宏，按顺序执行任意数量的形式，并返回最后一个形式的值：

    ```[lisp]
    (progn
        (...)
        (...))
    ```
3. **WHEN**宏，当条件满足时，按顺序执行一系列形式：

    ```[lisp]
    (when (condition) 
        (...)
        (...))
    ```

4. **UNLESS**宏，当条件不满足时，按顺序执行一系列形式：

    ```[lisp]
    (unless (condition)
        (...)
        (...))
    ```
5. **COND**宏，多重分支条件判断：
    
    ```[lisp]
    (cond
        (test-1 form*)
        (test-2 form*)
            .
            .
            .
        (test-N form*))
    ```

6. **AND**、**OR**和**NOT**，**“短路”**与、或、非：

    ```[lisp]
    (not nil)               ; T
    (not (= 1 1))           ; NIL
    (and (= 1 2) (= 3 3))   ; NIL
    (or (= 1 2) (= 3 3))    ; T
    ```

7. **DOLIST**和**DOTIMES**宏：

    1. **DOLIST**在一个列表的元素上循环操作，使用一个依次持有列表中所有后继元素的变量来执行循环体：

        ```[lisp]
        (dolist (var list-form)
            body-form*)
        ```
    2. **DOTIMES**用于循环计数的高级循环构造：

        ```[lisp]
        (dotimes (var count-form)
            body-form*)
        ```

8. **DO**宏：

    ```[lisp]
    (do (variable-definition*)
        (end-test-form result-form*)
        statement*)
    ```
9. **LOOP**宏：

    ```[lisp]
    ;; 简化版的LOOP
    (loop
        body-form*)

    ;;; 扩展版的LOOP

    ;; 生成一个从1到10的列表
    (loop for i from 1 to 10 collecting i)
    ; 结果为 (1 2 3 4 5 6 7 8 9 10)

    ;; 对前十个数求平方和
    (loop for x from 1 to 10 summing (expt x 2))
    ; 结果为 385

    ;; 用来统计一个字符串中元音字母的个数
    (loop for x across "the quick brown fox jumps over the lazy dog"
        counting (find x "aeiou"))
    ; 结果为 11
    ```

    **符号`across`、`and`、`below`、`collecting`、`counting`、`finally`、`for`、`from`、`summing`、`then`、`to`都是循环关键字，它们的存在表明当前正在使用扩展的LOOP。**

##8.自定义宏
1. **DEFMACRO**的基本框架：

    ```[lisp]
    (defmacro name (parameter*)
        "Optional documentation string."
        body-form*)
    ```

2. 宏的工作是将宏形式转换成做特定事情的代码。

3. 编写宏的步骤如下：

    1. 编写示例的宏调用以及它应当展开成的代码，反之亦然
    2. 编写从示例调用的参数中生成的手写展开式代码
    3. 确保宏抽象不产生“泄漏”

4. 反引用表达式例子

    |反引用语法|等价的列表构造代码|结果|
    |:---:|:---:|:---:|
    |`(a (+ 1 2) c)|(list 'a '(+ 1 2) 'c)|(a (+ 1 2) c)|
    |`(a ,(+ 1 2) c)|(list 'a (+ 1 2) 'c)|(a 3 c)|
    |`(a (list 1 2) c)|(list 'a '(list 1 2) 'c|(a (list 1 2) c)|
    |`(a ,(list 1 2) c)|(list 'a (list 1 2) 'c)|(a (1 2) c)|
    |`(a ,@(list 1 2) c)|(append (list 'a) (list 1 2) (list 'c))|(a 1 2 c)|

5. 预先堵上宏漏洞的规则：

    - 除非有特殊理由，否则需要将展开式中的任何子形式放在一个位置上，使其求值顺序与宏调用的子形式相同。
    - 除非有特殊理由，否则需要确保子形式仅被求值一次，方法是在展开式中创建变量来持有求值参数形式所得到的值，然后在展开式中所有需要用到该值的地方使用这个变量。
    - 在宏展开期使用GENSYM来创建展开式中用到的变量名。

##9. 数字、字符和字符串
1. 在浮点数中，指数标记s、f、d、l（以及它们等价的大写形式）分别代表短型、单精度、双精度以及长型浮点数。字母e代表默认表示方式（单浮点数）。

2. 复数用`#c`或`#C`跟上一个由两个实数所组成的列表表示，其分别代表实数的实部和虚部。如：`#c(2 1)`或`#(2/3 3/4)`。

3. `+-*/`当只有一个参数的时候，`+`和`*`直接返回其值，`-`返回其相反的值，而`/`返回其倒数。

4. 因为`/`不作截断处理，所以 Common Lisp 提供了4种类型的截断和舍入用于将一个数（有理数或浮点数）转化成整数：

    1. `FLOOR`向负无穷方向截断，返回小于或等于实参的最大整数
    2. `CEILING`向正无穷方向截断，返回大于或等于参数的最小整数
    3. `TRUNCATE`向零截断，对于正实参而言，它等价于`FLOOR`，而对于负实参而言则等价于`CEILING`
    4. `ROUND`舍入到最接近的整数上，如果参数刚好位于两个整数之间，它舍入到最接近的偶数上

5. `MOD`和`REM`函数：

    - `MOD`与`FLOOR`的关系`(+ (* (floor (/ x y)) y) (mod x y)) ≡ x`
    - `REM`与`TRUNCATE`的关系`(+ (* (truncate (/ x y)) y) (rem x y)) ≡ x`
    - 由上，对于正的商它们是等价的，而对于负的商它们产生不同的结果

6. 函数`1+`和`1-`提供了表示从一个数增加或者减少一个的简化方式。注意它们与`INCF`和`DECF`宏的不同：

    ```
    (incf x)        ≡   (setf x (1+ x)) ≡   (setf x (+ x 1))
    (decf x)        ≡   (setf x (1- x)) ≡   (setf x (- x 1))
    (incf x 10)     ≡   (setf x (+ x 10))
    (decf x 10)     ≡   (setf x (- x 10))
    ```

7. `/=`→不等于

8. `ZEROP`、`MINUSP`、`PLUSP`用来测试单一实数是否等于、小于或者大于零

9. `EVENP`和`ODDP`测试单一整数参数是否是偶数或者奇数

10. 高等数学

    |函数名|作用|
    |:---:|:---:|
    |LOG|求对数|
    |EXP|求e的指数|
    |EXPT|求一个参数的指数|
    |SIN|求sin值|
    |COS|求cos值|
    |TAN|求tan值|
    |ASIN|求asin值|
    |ACOS|求acos值|
    |ATAN|求atan值|

11. 字符的表示法：`#\`后跟想要的字符

12. 字符的比较：**大小写有关**的字符比较`CHAR=`和**大小写无关**的字符比较`CHAR-EQUAL`，其余的字符串比较符遵循了相同的命名模式：**大小写相关的比较符通过在其对应的数值比较符前面加上`CHAR`来命名；大小写无关的版本拼出比较符的名字，并在前面加上`CHAR-`**，不过，`<=`和`>=`被拼成了其逻辑等价形式`NOT-GREATERP`和`NOT-LESSP`，如下表：

    |数值相似物|大小写相关|大小写无关|
    |:---:|:---:|:---:|
    |=|CHAR=|CHAR-EQUAL|
    |/=|CHAR/=|CHAR-NOT-EQUAL|
    |<|CHAR<|CHAR-LESSP|
    |>|CHAR>|CHAR-GREATERP|
    |<=|CHAR<=|CHAR-NOT-GREATERP|
    |>=|CHAR>=|CHAR-NOT-LESSP|

13. 字符串的比较：

    |数值相似物|大小写相关|大小写无关|
    |:---:|:---:|:---:|
    |=|STRING=|STRING-EQUAL|
    |/=|STRING/=|STRING-NOT-EQUAL|
    |<|STRING<|STRING-LESSP|
    |>|STRING>|STRING-GREATERP|
    |<=|STRING<=|STRING-NOT-GREATERP|
    |>=|STRING>=|STRING-NOT-LESSP|

    - **字符串比较符只能比较两个字符串**，这是因为它们还带有关键字参数，从而允许你将比较限制在每个或两个字符串的子字符串上，如下：

        ```[lisp]
        (string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start 2 4 :end2 7)
        ; 结果比较子字符串"bar"，并返回真
        ```

    - 当参数不同时返回真的比较符，也即`STRING=`和`STRING-EQUAL`之外的所有操作符，将返回第一个字符串中首次检测到不匹配的索引：

        ```[lisp]
        (string/= "lisp" "lissome")
        ; 返回值为3
        ```

#10. 集合
1. 使用函数`VECTOR`来生成含有特定值的定长向量，该函数接受任意数量的参数并返回一个新分配的含有哪些参数的定长向量：

    ```[lisp]
    (vector)        ; #()
    (vector 1)      ; #(1)
    (vector 1 2)    ; #(1 2)
    ```

2. `MAKE-ARRAY`比`VECTOR`更加通用，因为它可以用来创建任何维度的数组以及定长和变长向量。

3. 创建一个可任意变长的向量：

    ```[lisp]
    (make-array 5 :fill-pointer 0 :adjustable t)
    ; 返回值 #()
    ```

4. 创建一个变长字符串：

    ```[lisp]
    (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
    ; 返回值 ""
    ```

5. 操作序列的函数：

    |名称|所需参数|返回|
    |:---:|:---:|:---:|
    |LENGTH|一个序列|序列的长度|
    |ELT|序列和位置|返回位于该位置的序列的元素内容|
    |COUNT|项和序列|序列中出现某项的次数|
    |FIND|项和序列|项或NIL|
    |POSITION|项和序列|序列中的索引或NIL|
    |REMOVE|项和序列|项的实例被移除后的序列|
    |SUBSTITUTE|新项、项和序列|项的实项被新项替换后的序列|

6. 标准序列函数关键字参数

    |参数|含义|默认值|
    |:---:|:---:|:---:|
    |:test|两参数函数用来比较元素（或由:key函数解出的值）和项|EQL|
    |:key|单参数函数用来从实际的序列元素中解出用于比较的关键字<br>NIL表示原样采用序列元素|NIL|
    |:start|子序列的起始索引（含）|0|
    |:end|子序列的终止索引（不含）。NIL表示到序列的结尾|NIL|
    |:from-end|如果为真，序列将以相反的顺序遍历，从尾到头|NIL|
    |:count|数字代表需要移除或替换的元素个数，NIL代表全部。<br>（仅用于REMOVE和SUBSTITUTE）|NIL|

7. `REMOVE-DUPLICATES`函数用来删除序列中重复的元素，只保留一个实例

    ```[lisp]
    (remove-duplicates #(1 2 1 2 3 1 2 3 4))
    ; 返回值为 #(1 2 3 4)
    ```

8. 整个序列上的操作：

    |函数名|参数|返回值|
    |:---:|:---:|:---:|
    |COPY-SEQ|单一的序列|返回包含与其参数相同元素的新对象|
    |REVERSE|单一的序列|返回含有顺序相反的相同元素的新对象|
    |CONCATENATE|序列类型和任意数量的序列|创建一个将任意数量序列连接在一起的新序列|
    
9. 函数`SORT`和`STABLE-SORT`提供了两种序列排序方法，它们都接受一个序列和一个由两个实参组成的谓词，返回该序列排序后的版本。它们的区别在于：**`STABLE-SORT`可以保证不会重排任何被该谓词视为等价的元素，而`SORT`只保证结果是已排序的并可能重排等价元素**

10. 函数`MERGE`接受两个序列和一个谓词，并返回按照该谓词合并这两个序列所产生的序列。

    ```[lisp]
    (merge 'vector #(1 3 5) #(2 4 6) #'<)
    ; 返回值为 #(1 2 3 4 5 6)

    (merge 'list #(1 3 5) #(2 4 6) #'<)
    ; 返回值为 (1 2 3 4 5 6)
    ```

11. 子序列操作：

    1. `SUBSEQ`函数：

        ```[lisp]
        (subseq "foobarbaz" 3)
        ; 返回值为 "barbaz"

        (subseq "foobarbaz" 3 6)
        ; 返回值为 "bar"

        (defparameter *x* (copy-seq "foobarbaz"))

        (setf (subseq *x* 3 6) "xxx")
        ; 效果 *x* → "fooxxxbaz"

        (setf (subseq *x* 3 6) "abcd")
        ; 效果 *x* → "fooabcbaz"

        (setf (subseq *x* 3 6) "xx")
        ; 效果 *x* → "fooxxcbaz"
        ```
    2. `FILL`函数将一个序列的多个元素设置到单个值上：

        ```[lisp]
        (fill "abc" #\d)
        ; 返回值 "ddd"

        (fill "abc" #\d :start 1 :end nil)
        ; 返回值 "add"
        ```

    3. `SEARCH`函数在一个序列中查找一个子序列：

        ```[lisp]
        (search "bar" "foobarbaz")
        ; 返回值为 3
        ```

    4. `MISMATCH`函数，找到两个带有相同前缀的序列首次不同的位置：

        ```[lisp]
        (mismatch "foobarbaz" "foom")
        ; 返回值为 3
        ```
12. 序列谓词，`EVERY`、`SOME`、`NOTANY`和`NOTEVERY`，它们在序列上迭代并测试一个布尔谓词：

    |名称|返回值|
    |:---:|:---:|
    |EVERY|在谓词失败时返回假，如果谓词全部都满足，则返回真|
    |SOME|返回由谓词所返回的第一个非NIL值<br>或者在谓词永远得不到满足返回假|
    |NOTANY|在谓词满足时返回假，或者从未满足时返回真|
    |NOTEVERY|在谓词失败时返回真，谓词总是满足时返回假|

13. 序列映射函数：
    
    1. `MAP`，接受一个n-参数函数和n个序列。`MAP`不返回布尔值，而是返回一个新序列，它是由那些将函数应用在序列的相继元素上所得到的结果组成：

        ```[lisp]
        (map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
        ; 返回值为 #(10 18 24 28 30)
        ```

    2. `MAP-INTO`和`MAP`相似，但它不产生给定类型的新序列，而是将结果放置在一个作为第一个参数传递的序列中：

        ```[lisp]
        (map-into a #'+ a b c)
        ; 效果为 将向量a、b、c相加并将结果放置到a中
        ```

    3. `REDUCE`函数，映射在单个序列上，先将一个两参数函数应用到序列的最初两个元素上，再将函数返回值和序列后续元素继续用于该函数：

        ```[lisp]
        (reduce #'+ #(1 2 3 4 5 6 7 8 9 10))
        ; 返回值为 55
        ```

14. 哈希表：

    1. 使用`MAKE-HASH-TABLE`将创建一个哈希表，其认定两个键等价，当且仅当它们在`EQL`的意义上是相同的对象。如果你想用字符串作为键，则你需要创建一个所谓的**EQUAL哈希表**，通过`:test`关键字来实现：

        ```[lisp]
        (defparameter *x* (make-hash-table))

        (defparameter *y* (make-hash-table :test #'equal))
        ```

    2. 使用`GETHASH`来获得哈希表中的一个值：

        ```[lisp]
        (gethash 'key *h*)
        ```

    3. 使用`REMHASH`来移除一个键值对：

        ```[lisp]
        (remhash 'key *h*)
        ```
    
    4. 使用`MAPHASH`来进行哈希表的迭代，其接受一个两参数函数和一个哈希表，并在哈希表的每一个键值对上调用该函数：

        ```[lisp]
        (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
        ; 打印哈希表的每一个键值对信息

        ;; 上面代码等价于
        (loop for k being the hash-keys in *h* using (hash-value v)
            do (format t "~a => ~a~%" k v))
        ```