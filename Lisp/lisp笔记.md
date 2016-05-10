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

#11. 列表处理
1. 列表处理函数：

    |函数|描述|
    |:---:|:---:|
    |NTH|接受一个索引和一个列表，并返回列表中第n个（从0开始）元素|
    |NTHCDR|接受一个索引和一个列表，并返回调用`CDR`n次的结果|
    |LAST|返回列表的最后一个点对单元。带有一个整数参数时，返回最后n个点对单元|
    |BUTLAST|返回列表的一个副本，最后一个点对单元除外。<br>带有一个整数参数时，排除最后n个单元|
    |NBUTLAST|`BUTLAST`的回收性版本。可能修改并返回其参数列表但缺少可靠的副作用|
    |LDIFF|返回列表直到某个给定点对单元的副本|
    |TAILP|返回真，如果给定对象是作为列表一部分的点对单元|
    |LIST*|构造一个列表来保存处最后一个参数外的所有参数，<br>然后让最后一个参数成为这个列表最后一个<br>节点的CDR。换而言之，它组合了`LIST`和`APPEND`|
    |MAKE-LIST|构造一个n项的。该列表的初始元素是NIL或<br>通过`:initial-element`关键字所指定的值|
    |REVAPPEND|`REVERSE`和`APPEND`的组合|
    |NRECONC|`REVAPPEND`的回收性版本，没有可靠的副作用|
    |CONSP|用来测试一个对象是否为点对单元的谓词|
    |ATOM||用来测试一个对象是否不是点对单元的谓词|
    |LISTP|用来测试一个对象是否为点对单元或NIL的谓词|
    |NULL|用来测试一个对象是否为NIL的谓词。<br>功能上等价于`NOT`但在测试空列表而非布尔假时推荐使用|

2. 映射

    |函数|描述|
    |:---:|:---:|
    |MAPCAR|与`MAP`类似，但总返回一个列表，因此无需提供类型实参，<br>只需提供想应用的函数和列表即可|
    |MAPLIST|与`MAPCAR`类似，区别在于`MAPLIST`传递给函数的是点对单元，<br>而`MAPCAR`值传递点对单元中 CAR 的部分|
    |MAPCAN|与`MAPCAR`类似，但会构造一个全新的列表来保存函数调用的结果|
    |MAPCON|与`MAPLIST`类似，但会构造一个全新的列表来保存函数调用的结果|
    |MAPC|是伪装成函数的控制构造，`MAPCAR`和`MAPCAN`的近亲，<br>只有当映射函数的副作用有用时，它才有用|
    |MAPL|是伪装成函数的控制构造，`MAPLIST`和`MAPCON`的近亲，<br>只有当映射函数的副作用有用时，它才有用|

#12. 点对单元的其他用法：
1. 树

    |函数|描述|
    |:---:|:---:|
    |COPY-TREE|为参数中的每个点对单元（包括点对单元中引用的点对单元）<br>生成一个新的点对单元，以相同的结构连接，并返回|
    |TREE-EQUAL|比较两棵树，当这两棵树具有相同的形状且对应叶子<br>是EQL等价时，返回真（含:test关键字）|
    |SUBST|接受一个新项、一个旧项和一棵树，并用新项替换旧项|
    |SUBST-IF|接受一个新项、一个单参数函数和一棵树，<br>满足但参数函数测试条件的项将被新项替换|

2. 集合

    |函数|描述|
    |ADJOIN|创建集合，接受一个项和一个代表集合的列表，<br>并返回另一个代表集合的列表，其中含有该项和原先集合的项|
    |PUSHNEW|接受一个项和一个集合，向集合中添加新项|
    |MEMBER|接受一个项，测试给定项是否在集合中|
    |MEMBER-IF|接受一个单参数函数，测试满足函数的项是否在集合中，<br>返回含有该项的点对单元|
    |MEMBER-IF-NOT|接受一个单参数函数，测试不满足函数的项是否在集合中，<br>返回含有该项的点对单元|
    |INTERSECTION|接受两个列表，返回一个由两个参数中可找到的所有元素组成的列表|
    |UNION|接受两个列表，返回一个含有来自两个参数的每个唯一元素的实例|
    |SET-DIFFERENCE|接受两个列表，返回一个含有来自第一个列表但不出现在第二个列表的所有元素|
    |SET-EXCLUSIVE-OR|接受两个列表，返回仅来自两个参数列表中的一个而不是两者的那些元素|
    |SUBSETP|接受两个列表，当第一个列表是第二个列表的子集时返回真|

3. 查询表：alist 和 plist 

    1. alist 的主查询函数是`ASSOC`，其接受一个键和一个 alist 并返回第一个 CAR 匹配该键的点对单元，或是在没有找到匹配时返回 NIL。

        ```[lisp]
        (assoc 'a '((a . 1) (b . 2) (c . 3)))
        ; 返回值为 (A . 1)

        (assoc 'd '((a . 1) (b . 2) (c . 3)))
        ; 返回值为 NIL
        ```

    2. `COPY-ALIST`与`COPY-TREE`相似，但它只复制那些构成列表结构的点对单元，外加那些单元的 CAR 部分直接引用的点对单元。

    3. `PAIRLIS`函数，从两个分开的键和值的列表中构造一个 alist：

        ```[lisp]
        (pairlis '(a b c) '(1 2 3))
        ; 返回值 可能为 ((C . 3) (B . 2) (A . 1))
        ; 也可能为 ((A . 1) (B . 2) (C . 3))
        ```

    4. plist 仅支持一种基本查询操作，`GETF`函数，其接受一个 plist 和一个键，返回所关联的值或是在键没有被找到时返回 NIL。

    5. plist 使用`REMF`宏来进行移除一个键/值对，如果给定键被找到并移除成功则返回 T ，否则返回 NIL。

    6. plist 使用`GET-PROPERTIES`来抽取多个值，它接受一个 plist 和一个需要被搜索的键的列表，并返回多个值：第一个被找到的键、其对应的值，以及一个以被找到到的键开始的列表的头部。

4. **DESTRUCTURING-BING**宏

    1. 基本骨架：

        ```[lisp]
        (destructuring-bind (parameter*) list
            body-form*)
        ```

    2. 其作用是将一个原本绑定在单个参数上的列表拆开。其中的 list 形式被求值一次并且应当返回一个列表，其随后被解构并且适当的值会被绑定到形参列表的对应变量中，然后那些 body-form 将在这些绑定的作用下被求值：

        ```[lisp]
        (destructuring-bind (x y z) (list 1 2 3)
            (list :x x :y y :z z))
        ; 返回值为 (:X 1 :Y 2 :Z 3)

        (destructuring-bind (x y z) (list 1 (list 2 20) 3)
            (list :x x :y y :z z))
        ; 返回值为 (:X 1 :Y (2 20) :Z 3)

        (destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
            (list :x x :y1 y1 :y2 y2 :z z))
        ; 返回值为 (:X 1 :Y1 2 :Y2 20 :Z 3)

        (destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
            (list :x x :y1 y1 :y2 y2 :z z))
        ; 返回值为 (:X 1 :Y1 2 :Y2 NIL :Z 3)

        (destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
            (list :x x :y y :z z))
        ; 返回值为 (:X 1 :Y 2 :Z 3)

        (destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
            (list :x x :y y :z z))
        ; 返回值为 (:X 3 :Y 2 :Z 1)
        ```

#13. 文件和文件 I/O
1. `OPEN`函数，用于打开文件：

    ```[Lisp]
    (open "/some/file/name.txt")
    ; 返回值为一个基于字符的流，你可以用下面的函数来读取其中内容
    ; 1. READ-CHAR  读取单个字符
    ; 2. READ-LINE  读取一行文本
    ; 3. READ       读取单一的S-表达式并返回一个 Lisp 对象
    ```

2. 读取二进制数据：为了读取原始字节，你需要向`OPEN`传递一个值为`'(unsigned-byte 8)`的`:element-type`参数，之后你可以将得到的流传给`READ-BYTE`，它将在每次被调用的时候返回 0~255 的整数

3. 批量读取：`READ-SEQUENCE`可同时工作在字符和二进制流上。你传递给它一个序列（通常是一个向量）和一个流，然后它会尝试用来自流的数据填充该序列

4. 文件输出：通过在`OPEN`时使用一个值为`:output`的`:direction`关键字参数来获取输出流。当你打开一个用于输出的文件时，`OPEN`会假设该文件不该存在并会在文件存在时报错。你可以使用`:if-exists`关键字来改变该行为，传递`:supersede`可以告诉`OPEN`来替换已有的文；传递`:append`将导致`OPEN`打开已有的文件并保证新数据被写到文件结尾处；而`:overwrite`返回一个从文件开始处开始的流从而覆盖已有的数据；传递`NIL`将导致`OPEN`在文件已存在时返回`NIL`而不是流。如下：

    ```[lisp]
    (open "some/file/name.txt" :direction :output :if-exists :supersede)
    ; 返回一个输出流，你可以用下面的函数来向其中写入内容
    ; WRITE-BYTE 向流中写入单独的字节
    ; WRITE-CHAR 会向流中写入一个单一字符
    ; WRITE-LINE 写一个字符串并紧跟着一个换行
    ; WRITE-STRING 写一个字符串而不会添加任何行结束符
    ; WRITE-SEQUENCE 同时接受二进制和字符流
    ; PRINT 打印一个 S-表达式，前缀一个换行及一个空格
    ; PRIN1 只打印 S-表达式
    ; PPRINT 使用美化打印器（pretty printer）打印 S-表达式
    ; PRINC 以适合人类的形式打印 Lisp对象，如： 字符串不带引号
    ;
    ; 同时，有两个函数可以只打印一个换行
    ; TERPRI 终止打印（terminate print）无条件地打印一个换行符
    ; FRESH-LINE 打印一个换行字符，除非该流已经在一行的开始处
    ```

5. 确保文件使用后被关闭的宏`WITH-OPEN-FILE`：

    ```[lisp]
    (with-open-file (stream-var open-argument*)
        body-form*)
    ```

6. 路径名是一种使用6个组件来表示文件名的结构化对象：

    |对象名|对应函数|
    |:---:|:---:|
    |主机（host）|PATHNAME-HOST|
    |设备（device）|PATHNAME-DEVICE|
    |目录（directory）|PATHNAME-DIRECTORY|
    |名称（name）|PATHNAME-NAME|
    |类型（type）|PATHNAME-TYPE|
    |版本（version）|PATHNAME-VERSION|

    - `PATHNAME`函数可以得到一个路径名对象，并且可以通过上表函数来访问其信息

        ```[lisp]
        (pathname "/foo/bar/baz.txt")
        ; 返回值为 #P"/foo/bar/baz.txt"

        (pathname-directory (pathname "/foo/bar/baz.txt"))
        ; 返回值为 (:ABSOLUTE "foo" "bar")
        ```

    - `NAMESTRING`函数可以将一个路径名描述符转换成一个字符串，类似的还有`DIRECTORY-NAMESTRING`和`FILE-NAMESTRING`函数：

        ```[lisp]
        (namestring #p"/foo/bar/baz.txt")
        ; 返回值为 "/foo/bar/baz.txt"
        (directory-namestring #p"/foo/bar/baz.txt")
        ; 返回值为 "/foo/bar/"
        (file-namestring #p"/foo/bar/baz.txt")
        ; 返回值为 "baz.txt"
        ```

    - `MAKE-PATHNAME`函数来构造任意路径名，它对每个路径名组件都接受一个关键字参数并返回一个路径名，任何提供了的组件都被填入其中而其余的为 NIL：

        ```[lisp]
        (make-pathname
            :directory '(:absolute "foo" "bar")
            :name "baz"
            :type "txt")
        ; 返回值为 #P"/foo/bar/baz.txt"
        ```

    - 为了实现在不同平台下生成的文件路径都合法，应基于一个已有的路径名并使用`MAKE-PATHNAME`的关键字参数`:defaults`来构造一个新路径名：

        ```[lisp]
        (make-pathname :type "html" :version :newest :defaults input-file)
        ; input-file 默认为一个已经存在的文件路径名
        ; 该表达式创建了一个带有 .html 扩展名的路径名，同时所有其他组件
        ; 都与变量 input-file 中的路径名相同
        ; 这里 :version :newest 是为了支持那些带有版本号的文件系统
        ; 在没有文件版本的系统，如 Unix 和 Windows :version 参数会
        ; 被忽略
        ;
        ; 同时你还可以创建一个带有不同目录组件的路径名，如下
        (make-pathname :directory '(:relative "backups") :defaults input-file)
        ```

    - `MERGE-PATHNAMES`函数接受两个路径名，并合并它们，用来自第二个路径名的对应值填充第一个路径名中的任何NIL组件：

        ```[lisp]
        (merge-pathnames #p"foo/bar.html" #p"/www/html/")
        ; 返回值为 #p"/www/html/foo/bar.html"

        (merge-pathnames #p"foo/bar.html" #p"html/")
        ; 返回值为 #p"html/foo/bar.html"
        ```

    - `ENOUGH-NAMESTRING`函数接受两个路径名，返回第一个路径名对第二个路径名的相对路径：

        ```[lisp]
        (enough-namestring #p"/www/html/foo/bar.html" #p"/www/w")
        ; 返回值为 "html/foo/bar.html"
        ```

7. 与文件系统的交互

    1. `PROBE-FILE`函数用来测试一个对应于某个路径名描述符（路径名、名字字符串或文件流）的文件是否存在于文件系统，如果存在则返回该文件的真实名字（truename）一个进行了诸如解析符号链接这类文件系统层面转换的路径名。否则返回 NIL

    2. `DIRECTORY`函数用来列出文件系统中的文件

    3. `DELETE-FILE`接收一个路径名描述符并删除所命令的文件，当成功时返回真，否则产生一个`FILE-ERROR`报错

    4. `RENAME-FILE`接收两个路径名描述符，并将第一个名字命名的文件重命名为第二个名字

    5. `ENSURE-DIRECTORIES-EXIST`函数用来创建目录，它接受一个路径名描述符并确保目录组件中的所有元素存在并且是目录，如果必要的话它会创建它们，并返回被传递的路径名，因此它可以内联使用：

        ```[lisp]
        (with-open-file (out (ensure-directories-exist name) :direction :output :if-exist :supersede)
            ...
            )
        ```

    6. `FILE-WITH-DATE`函数接受一个路径名描述符，并返回文件上次被写入的时间，表示形式是从 GMT 起经过的秒数

    7. `FILE-AUTHOR`函数接受一个路径名描述符，在 Unix 和 Windows 上返回该文件的拥有者

    8. `FILE-LENGTH`函数接受一个流，返回该流的长度，获得一个文件长度的最佳方式是使用一个二进制流：

        ```[lisp]
        (with-open-file (in filename :element-type '(unsigned-byte 8))
            (file-length in))
        ```

    9. `FILE-POSITION`函数接受一个流，当只用一个流来调用它的时候，返回文件中的当前位置，即已经被读取或写入该流的元素的数量；当参数为两个时（流和位置描述符），它将流的位置设置到所描述的位置上，这个位置描述符必须是关键字`:start`、`:end`或者非负的整数

8. 其他 I/O 类型
    
    1. `STRING-STREAM`字符串流，从一个字符串中读取或写入数据。可以使用`MAKE-STRING-INPUT-STREAM`和`MAKE-STRING-OUTPUT-STREAM`来创建它。不过宏`WITH-INPUT-FROM-STRING`和`WITH-OUTPUT-TO-STRING`提供了更加便利的接口

    2. 语言标准中定义的其他流提供了多种形式的**流拼接技术**，它允许你已几乎任何配置将流拼接在一起

        - `BROADCAST-STREAM`是一个输出流，它将向其写入的任何数据发送到一组输出流上，这些流是它的构造函数`MAKE-BROADCAST-STREAM`的参数

        - `CONCATENATED-STREAM`是一个输入流，它从一组输入流中接受其输入，在每个流的结尾处它从一个流移动到另一个，可以使用函数`MAKE-CONCATENATED-STREAM`来构造它，接受任何数量的输入流作为参数

        - `TWO-WAY-STREAM`和`ECHO-STREAM`是可以将流以多种方式拼接在一起的双向流，它们的构造函数`MAKE-TWO-WAY-STREAM`和`MAKE-ECHO-STREAM`都接受两个参数，一个输入流和一个输出流，并返回一个适当类型的可同时用于输入和输出函数的流。在`TWO-WAY-STREAM`中，你所做的每一次读取都会返回从底层输入流中读取的数据，而每次写入将把数据发送到底层的输出流上。而在`ECHO-STREAM`中，除了所有从底层输入流中读取的数据也被回显到输出流中之外，它以与`TWO-WAY-STREAM`本质上相同的方式工作，这样，`ECHO-STREAM`中的输出流将含有会话双发的一个副本

#14 广义函数（generic function）
1. 广义函数定义了抽象操作，指定了其名字和一个参数列表，但不提供实现。例如，下面就是可能定义广义函数`draw`的方式，它将用来在屏幕上绘制不同的形状：

    ```[lisp]
    (defgeneric draw (shape)
        (:documentation "Draw the given shape on the screen."))
    ```

2. 使用`DEFMETHOD`来实现广义函数定义的方法，方法的形参必须和广义函数保持一致

