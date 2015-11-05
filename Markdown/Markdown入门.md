# Markdown 入门
参考资料：[掌握 Markdown](http://jingxuan.io/markdown/?hmsr=toutiao.io&utm_medium=toutiao.io&utm_source=toutiao.io)
## 基本语法
### 标题
____
`#`相当于`<h1>`标签，为一级标题
`##`相当于`<h2>`标签，为二级标题
...
`######`相当于`<h6>`标签，为六级标题

### 强调
____
`*`文本`*`  文本会变成斜体，如：\*你好\* --> *你好*
`_`文本`_` 文本也会变成斜体，如：\_你好\_ --> _你好_

`**`文本`**` 文本会变成粗体，如：\*\*你好\*\* --> **你好**
`__`文本`__` 文本也会变成粗体，如：\_\_你好\_\_ --> __你好__

你可以结合以上用法使其变成：***你好***

### 列表
____
#### 无序
**形如：**
```
* Item1
* Item2
　* Item2-1
　* Item2-2
```

**效果如下：**
* Item1
* Item2
  * Item2-1
  * Item2-2

#### 有序
**形如：**
```
1. Item1
2. Item2
3. Item3
  * Item3-1
  * Item3-2
```

**效果如下：**
1. Item1
2. Item2
3. Item3
  * Item3-1
  * Item3-2

### 插入图片
____
**形如：**
```
![Alt Text](picture url)
```
**效果如下：**
![Github Logo](https://assets-cdn.github.com/images/modules/dashboard/bootcamp/octocat_fork.png)

### 插入链接
----
**形如：**
```
[Text](url)
```

**效果如下：**
[Google](www.google.com)
[百度](www.baidu.com)

### 区块引用
----
**形如：**
```
> 你好
```
**效果如下：**
我说：
> Hello World

## GitHub 扩展的 Markdown语法
GitHub上使用的自己定制过的Markdown，扩展了很多有用的功能特性，这些特性和功能使得GitHub上的内容产生更加的容易。

注意：部分GitHub扩展的Markdown语法只能用在`Issues`和`Pull Requests`的描述和评论中， 像`@`某用户的功能。任务列表语法还可以用在Gist评论和Gist Markdown 文件里。

### 语法高亮
____
使用[GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown)进行语法高亮
**形如：**
\`\`\`\[编程语言(可省略)]
public static void main(String[] args) {
  System.out.println("Hello World");
}
\`\`\`
或直接用tab缩进

**效果如下：**
```[java]
public static void main(String[] args) {
  System.out.println("Hello World");
}
```

### 任务列表
____
**形如：**
```
\- [x] 完成的任务
\- [ ] 未完成的任务
```

### 表格
____
用如下的语法创建表格：
```
| Tables        | Are           | Cool  |
| ------------- |:-------------:| -----:|
| col 3 is      | right-aligned | $1600 |
| col 2 is      | centered      | $12   |
| zebra stripes | are neat      | $1    |
```
**效果如下：**

| Tables | Are | Cool |
| ------------- |:-------------:| -----:|
| col 3 is | right-aligned | $1600 |
| col 2 is | centered | $12 |
| zebra stripes | are neat | $1 |

## 补充语法
补充一些上面漏掉的但也很实用的语法
### 分割线：
____
**形如：**
```
---- 或 ***
```
**效果如下：**
***

### 删除线：
----
**形如：**
```
~~文本~~
```
**效果如下：**
~~文本~~

### 使用'\\'输出具有特殊功能的字符
如果我想输出`*`、`-`、\\等字符，需要用以下形式：
```
\* \- \\
```
**效果如下：**
\* \- \\
