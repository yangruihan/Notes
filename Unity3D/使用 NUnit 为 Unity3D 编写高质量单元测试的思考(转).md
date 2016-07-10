# 使用 NUnit 为 Unity3D 编写高质量单元测试的思考（转）
![](http://www.gameres.com/data/attachment/forum/201607/07/141214ffppju29rnj32gs9.jpg)

[原文地址](http://www.gameres.com/thread_668571_1_1.html)

## 一、单元测试 Pro & Con
最近尝试在我参与的游戏项目中引入TDD（测试驱动开发）的开发模式，因此单元测试便变得十分必要。这篇博客就来聊一聊这段时间的感悟和想法。由于 游戏开发和传统软件开发之间的差异，因此在开发游戏，特别是使用Unity3D开发游戏的过程中编写单元测试往往会面临两个主要的问题：

1. 游戏开发中会涉及到很多的I/O操作处理，以及视觉和UI的处理，而这个部分是单元测试中比较难以处理的部分

2. 具体到使用Unity3D开发游戏，我们自然而然的希望能够将测试的框架集成到Unity3D的编辑器中，这样更加容易操作

但是，单元测试的好处也十分多：

1. TDD，测试驱动开发。编写单元测试将使我们从调用者观察、思考。特别是先写测试，迫使我们把程序设计成易于调用和可测试的，即迫使我们解除软件中的耦合。可以将任务的粒度降低。当然TDD是否适合游戏开发尚有争论，但是单元测试的必要性是无需置疑的

2. 单元测试是一种无价的文档，它是展示方法或类如何使用的最佳文档。这份文档是可编译、可运行的，并且它保持最新，永远与代码同步

3. 更加适合应对需求的经常性变更。身处游戏开发行业的从业人员都不能否认的一点便是游戏开发中需求变更是一件不可避免甚至是必不可少的事情，而单元测试另一个好处便是一旦因为需求变更而出现bug，能够很快的发现，进而解决问题

## 二、Unity3D中常用的测试工具

针对问题1，由于对I/O处理以及UI视觉方面的操作比较难以实施单元测试，所以我们单元测试的主要对象是逻辑操作以及数据存取的部分

针对问题2，Unity5.3.x已经在editor中集成了测试模块。该测试模块依托了NUnit框架（NUnit是一个单元测试框架,专门针对于.NET来写的.其实在前面有JUnit(Java),CPPUnit(C++),他们都是xUnit的一员.最初,它是从JUnit而来.U3d使用的版本是2.6.4）

在Unity Editor中实现测试而不是在IDE中进行测试的原因在于，一些Unity的API需要在Unity的环境中来运行，而无法直接在外部的IDE中实现，例如实例化GameObject

而且除了Unity5.3.x自带的单元测试模块之外，Unity官方还推出了一款测试插件Unity Test Tool（基于[NSubstitute](http://nsubstitute.github.io/help/getting-started/)），除了单元测试之外还包括：

1. 单元测试

2. 集成测试

3. 断言组件

需要指出的是Unity Test Tool基于NSubstitute这个库

## 三、初识单元测试
既然本文的主题是单元测试，那么我们就必须先对单元测试下一个定义：

一个单元测试是一段自动化的代码，这段代码调用被测试的工作单元，之后对这个单元的单个最终结果的某些假设进行检验。单元测试使用单元测试框架编写，并要求单元测试可靠、可读并且可维护。只要产品代码不发生变化，单元测试的结果是稳定的

既然有了单元测试的定义，下面我们就尝试在Unity项目中写单元测试吧

一个单元测试的小例子：

编写单元测试用例时，使用的主要是Unity Editor自带的单元测试模块，因此单元测试是基于NUnit框架的。

借助NUnit，我们可以：

1. 编写结构化的测试

2. 自动执行选中的或全部的单元测试

3. 查看测试运行的结果

因此这就要求编写Unity3D项目的单元测试时，要**引入NUnit.Framework命名空间**，且单元测试类要**加上[TestFixture]属性**，单元测试方法要**加上[Test]属性**，并将测试用例的文件**放在Editor文件夹**下

下面是一个例子：

```csharp
using UnityEngine;
using System.Collections;
using NUnit.Framework;

[TestFixture]
public class HpCompTests
{
  //测试被攻击之后伤害数值是否和预期值相等
  [Test]
  public void TakeDamage_BeAttacked_HpEqual()
    {
      //Arrange
      HpComp health = new HpComp();
      health.currentHp = 100;
     //Act
      health.TakeDamage(50);
     //Assert
      Assert.AreEqual(50f, health.currentHp);
    }
}
```

该例子是测试英雄受到伤害之后，血量是否和预期的相等。

测试框架会创建这个测试用例类，并且调用`TakeDamage_BeAttacked_HpEqual`方法来和其交互，最后使用`Nunit的Assert`类来断言是否通过测试

## 四、单元测试的结构
通过上面的小例子，我们可以发现单元测试其实是有结构的。下面我们就来具体分析一下：

使用NUnit提供的特性来标识测试代码

NUnit使用C#的特性机制识别和加载测试。这些特性就像是书签，用来帮助测试框架识别哪些部分是需要调用的测试

如果要使用NUnit的特性，我们需要在测试代码中首先引入NUnit.Framework命名空间

而NUnit运行器至少需要两个特性才知道需要运行什么

1. [TestFixture]：标识一个自动化NUnit测试的类

2. [Test]：可以加在一个方法上，标识这个方法是一个需要调用的自动化测试

当然，还有一些别的特性供我们使用，来方便我们更好的控制测试代码，例如[Category]特性可以将测试分类、[Ignore]特性可以忽略测试

常用的NUnit属性见下表：

|属性名|
|:---:|
|[SetUp]|
|[TearDown]|
|[TestFixture]|
|[Test]|
|[TestCase]|
|[Category]|
|[Ignore]|

**测试命名和布局标准**

测试类的命名：

对应被测试项目中的一个类，创建一个名为`[ClassName]Tests`的类

工作单元的命名：

对每个工作单元（测试），测试方法的方法名由三部分组成，并且按照如下规则命名：`[被测试的方法名]_[测试进行的假设条件]_[对测试方法的预期]`

具体来说：

1. 被测试的方法名

2. 测试进行的假设条件，例如“登入失败”、“无效用户”、“密码正确”

3. 对测试方法的预期：在测试场景指定的条件下，我们对被测试方法的行为的预期

其中，对测试方法的预期会有三种可能的结果：

1. 返回一个值（数值、布尔值等等）

2. 改变被测试的系统的一个状态

3. 调用一个第三方系统

可以看出，我们的测试代码在格式上与标准的代码有所不同，测试名可以很长，但是在编写测试代码时，可读性是最为重要的方面之一，而测试名中的**下划线**可以令我们不会遗漏所有的重要信息，我们甚至可以将测试方法名当做一个句子来读，这样就会使得这个测试方法的测试目标、场景以及预期都十分明确，无需额外的注释

**测试单元的行为——3A原则**

有了NUnit属性可以标识可以自动运行的测试代码和测试代码的一些命名规则，下面我们就来看看如何测试自己的代码

一个单元测试通常包含三个行为，可以归纳为3A原则即：

1. Arrange，准备对象，创建对象并进行必要的设置

2. Act，操作对象

3. Assert，断言某件事情是预期的

下面是之前的那段简单的代码，包含了以上的NUnit的属性、命名规范以及3A原则下的行为，其中断言部分使用了NUnit框架提供的`Assert`类，被测试的类为`HpComp`，被测试的方法为`TakeDamage`

```csharp
using NUnit.Framework;

[TestFixture]
public class HpCompTests
{
  //测试被攻击之后伤害数值是否和预期值相等
  [Test]
  public void TakeDamage_BeAttacked_HpEqual()
  {
      //Arrange
      HpComp health = new HpComp();
      health.currentHp = 100;
      //Act
      health.TakeDamage(50);
      //Assert
      Assert.AreEqual(50f, health.currentHp);
  }
}
```

**单元测试的断言——Assert类**

NUnit框架提供了一个Assert类来处理断言的相关功能。Asset类用于声明某个特定的假设应该成立，因此如果传递给Assert类的参数和我们断言（预期）的值不同，则NUnit框架会认为测试没有通过。

Assert类会提供一些静态方法，供我们使用。

例如：

```csharp
Assert.AreEqual(预期值，实际值)；
Assert.AreEqual(1，2 - 1)；
```

关于Assert类的静态方法，各位可以直接在代码中看

## 单元测试的可靠性
我们的目标是写出可靠、可维护、可读的测试。

因此，除了遵循单元测试结构规范编写单元测试之外，我们还需要注意可靠性、可维护性以及可读性这些方面。因此，一些原则我们也需要注意

**不轻易删除和修改测试**

一旦测试写好了并且通过了，就不应该轻易的修改和删除这些测试。因为这些测试是对应系统代码的保护伞，在修改系统代码时，这些测试会告诉我们修改后的代码是否会破坏已有的功能

**尽量避免测试中的逻辑**

随着测试中的逻辑增多，测试代码出现缺陷的几率也会增大。而且由于我们往往相信测试是可靠的，因此一旦测试出现缺陷我们往往不会首先考虑是测试的问题，可能会浪费时间去修改系统代码。而单元测试中，最好保持逻辑的简单，因此尽量避免使用下面的逻辑控制代码

1. `switch`、`if`

2. `foreach`、`for`、`while`

一个单元测试应该是一系列的方法调用和断言，但是不应该包含控制流语句

**只测试一个关注点**

在一个单元测试中验证多个关注点会使得测试代码变得复杂，但却没有价值。相反，我们应该在分开的、独立的单元中验证多余的关注点，这样才能发现真正导致失败的地方

## 六、单元测试的可维护性
**去除重复代码**

和系统中的重复代码一样，在单元测试中重复代码同样意味着测试对象某方面改变时要修改更多的测试代码。

如果测试看上去都一样，仅仅是参数不同，那么我们完全可以使用参数化测试即使用[TestCase]特性将不同的数据作为参数传入测试方法

**实施测试隔离**

所谓的测试隔离，指的是一个测试和其他的测试隔离，甚至不知道其他测试的存在，而只在自己的小世界中运行。

将测试隔离的目的是防止测试之间的互相影响，常见的测试之间互相影响的情况可以总结如下：

1. 强制的测试顺序：测试要以某种顺序执行，后一个测试需要前面的测试结果，这种情况有可能会导致问题的原因是因为NUnit不能保证测试按照某种特定的顺序执行，因此今天通过的测试，明天可能就不好用了

2. 隐藏的测试调用：测试调用其他测试

3. 共享状态被破坏：测试要共享状态，但是在一个测试完成之后没有重置状态，进而影响后面的测试

## 七、单元测试的可读性
正如概述中所说单元测试是一种无价的文档，它是展示方法或类如何使用的最佳文档。因此，可读性这条要求的重要性便可见一斑。试想一下即便是几个月之后别的程序员都可以通过单元测试来理解一个系统的组成以及使用方法，并能够很快的理解他们要做的工作以及在哪里切入

**单元测试命名**

在单元测试的结构中已经有过要求和介绍。参考那部分。

**单元测试中的变量命名**

通过合理的命名变量，可以提高可读性，使得阅读测试的人员可以尽快的理解你要验证的内容。

还是看看上面的例子

```csharp
[Test]
public void TakeDamage_BeAttacked_HpEqual()
{
    //Arrange
    HpComp health = new HpComp();
    health.currentHp = 100;
    //Act
    health.TakeDamage(50);
    //Assert
    Assert.AreEqual(50f, health.currentHp);
}
```

这段代码中的断言使用了一个魔数50，但是这个数字并没有使用描述性的名字，因此我们无法尽快的知道这个数字预期的是什么。因此，我们尽可能不要直接使用数字和结果比较，而是使用一个有意义命名的变量来和结果进行比较

```csharp
[Test]
public void TakeDamage_BeAttacked_HpEqual()
{
    HpComp health = new HpComp();
    health.currentHp = 100;

    health.TakeDamage(50);

    float leftHp = 50f;

    Assert.AreEqual(leftHp, health.currentHp);
}
```

## 八、在Untiy编辑器中写单元测试
在Unity编辑器中编写单元测试用例时，使用的主要是Unity编辑器自带的单元测试模块，因此单元测试是基于NUnit框架的

这就要求编写单元测试时，要引入NUnit.Framework命名空间，且单元测试类要加上[TestFixture]属性，单元测试方法要加上[Test]属性，并将测试用例的文件放在Editor文件夹下

测试用例的编写结构要遵循3A原则，即Arrange, Act, Assert

即先要设置测试环境，例如实例化测试类，为测试类的字段赋值

之后写测试的行为

最后是判断是否通过测试

下面是一个例子：

```csharp
using UnityEngine;
using System.Collections;
using NUnit.Framework;

[TestFixture]
public class HealthComponentTests
{
  //测试伤害之后，血的值是否比0大
  [Test]
  public void TakeDamage_BeAttacked_BiggerZero()
    {
      //Arrange
      UnMonoHealthClass health = new UnMonoHealthClass();
      health.healthAmount = 50f;

      //Act
      health.TakeDamage(60f);

      //Assert
      Assert.GreaterOrEqual(health.healthAmount, 0);
    }
}
```

该例子是测试英雄受到伤害之后，血量是否会越界出现负值。

测试框架会创建这个测试用例类，并且调用`TakeDamage_BeAttacked_BiggerZero`方法来和其交互，最后使用`Nunit的Assert`类来断言是否通过测试

**使用Editor Tests Runner开始单元测试：**

写完了单元测试用例之后，我们就可以在Unity5.3.x的editor中开始单元测试了。如图所示：

![](http://www.gameres.com/data/attachment/forum/201607/07/141214hqcrqyykrqcgkutu.png)

![](http://www.gameres.com/data/attachment/forum/201607/07/141214y6yq56dyyydqlk4f.png)

在这里，我们既可以跑单独的测试用例，也可以跑所有的测试用例，通过的是绿色标识，未通过的是红色标识

而在最上面的一行，则是我们可以操作的部分：

- Run All：测试全部用例

- Run Selected：测试选中的用例

- Rerun Failed： 重新测试上一次未通过的测试用例

- 搜索框：可以搜索用例

- 种类过滤器：可以根据种类来筛选用例。种类需要在测试代码中使用CategoryAttribute来标识。

- 测试结果筛选器：可以按照通过、失败以及忽略来筛选用例

![](http://www.gameres.com/data/attachment/forum/201607/07/141214r39zpo3r4d31nmze.png)

在这里我们还可以设置在编译前自动运行单元测试

**使用命令行运行单元测试：**

除了能够在Editor中使用单元测试，我们自然更希望能够将单元测试也纳入自动集成的流水线中，因此有必要从U3D外部调用测试。不过好在U3D也提供了外部调用的方式，这样将单元测试也加入到我们的自动集成的流水线中是可行的

Unity3D 5.3.x版本中提供的命令行选项如下：

```csharp
runEditorTests  必须，运行editor test的选项
editorTestsResultFile 用来保存测试结果
editorTestsFilter 根据用例名称，来运行指定的用例
editorTestsCategories 根据用例种类，来运行指定的用例
editorTestsVerboseLog 打印更加详细的日志
projectPath 工程目录
所以在命令行中开启测试可以这样写：
Unity -runEditorTests -projectPath /Users/fanyou/UnitTest -editorTestsResultFile  /Users/fanyou/UnitTest/test.xml -batchmode -quit
```

## 九、后记
以上便是关于在U3D中引入单元测试的一些思考，当然，游戏开发是否适合TDD，换言之是否要先写单元测试后实现功能是值得讨论的事情，但是单元测试本身是十分有必要在工程中使用的。在代码结构设计、日后的重构都会很有帮助

相关阅读：[TDD在Unity3D游戏项目开发中的实践](http://www.gameres.com/497930.html)
