# Python 序列化模型数据为 JSON
相信使用 Python 做 Web 开发的朋友都会遇到这样 1 个问题，那就是在项目开发中使用模型框架，比如 SQLAlchemy、Peewee，我们在做 RESTful 接口时如何将这些模型序列化为 JSON 数据

关于这个问题，跟隔壁那位搞 Python 的哥们有关系。我不得不佩服这位哥们竟然自己写了 1 套 ORM 框架，而且用起来的那么遛，不得不让我汗颜

但是，在给前端提供接口的时候，如何序列化为JSON数据确实困扰了我们那么一阵子，毕竟占据我们很大一部分时间来进行序列化操作

这里,我们使用 peewee 来定义 1 个简单的例子来说明:

```python
from peewee import SqliteDatabase
from peewee import Model, CharField, DateField, BooleanField, ForeignKeyField

db = SqliteDatabase('dev.sqlite3')

class BaseModel(Model):
    class Meta:
        database = db

class Person(BaseModel):
    name = CharField(max_length= 20)
    birthday = DateField()
    sex = BooleanField()

class Pet(BaseModel):
    owner = ForeignKeyField(Person, related_name= 'pets')
    name = CharField(max_length= 10)
    animal_type = CharField(max_length= 20)
```

在这里我们定义了Person和Pet这2个模型,每个Person可能有1个Pet的宠物

我们插入一些数据,现在假设我们现在有如下的数据:

```python
sqlite> select * from person;
1|Bob|1960-01-15|1
2|Grandma|1935-03-01|0
3|Herb|1950-05-05|1
sqlite> select * from pet;
1|1|Kitty|cat
2|3|Fido|dog
3|3|Mittens|cat
4|2|Jack|cat
```

现在,我们假设我们接口需要返回的接口是每个用户的名称、生日及其对应的宠物的信息

我们可以通过连表的方式轻松的获取到我们需要的数据:

```python
query=Person.select(Person,Pet).join(Pet)
```

那么我们怎么将这个模型数据转换为我们需要的 JSON 数据呢?一般情况下,我们会这样操作:

```python
data = []
for person in query.aggregate_rows():
    d={}
    d['username'] = person.name
    d['birthday'] = person.birthday
    d['pet'] = []
    for pet in person.pets:
        o = {}
        o['name'] = pet.name
        o['animal_type'] = pet.animal_type
        d['pet'].append(o)
    data.append(d)
```

最后我们将得到如下的结果:

```python
[{'birthday': datetime.date(1960, 1, 15),
  'pet': [{'animal_type': u'cat', 'name': u'Kitty'}],
  'username': u'Bob'},
 {'birthday': datetime.date(1950, 5, 5),
  'pet': [{'animal_type': u'dog', 'name': u'Fido'},
   {'animal_type': u'cat', 'name': u'Mittens'}],
  'username': u'Herb'},
 {'birthday': datetime.date(1935, 3, 1),
  'pet': [{'animal_type': u'cat', 'name': u'Jack'}],
  'username': u'Grandma'}]
```

可以看到,这么1个简单的例子,我们已经对序列化操作处理的已经够呛的。对于那些更为复杂的模型,我们预计只有哭的份了

因此,我们希望能找到1个库可以减轻我们的工作量,于是我们找到了1个marshallow的库。
下面我们来说说如何使用marshallow来减轻序列化模型的工作量

主要包括如下2个步骤:

- 定义模式

- 序列化模型

下面我们分别来看看

## 定义模式

如果你使用过 Flask-RESTful，你应该知道该库提供了1个`marshal_with`的函数。其中我们就需要定义我们给定字段返回的数据类型,但是 Flask-RESTful 没有提供字段不同返回的操作

我们通过如下的方式导入模式及其对应的字段:

```python
from marshmallow import Schema, fields
```

接下来,我们定义 1 个继承自 Schema 的类,然后定义其对应的字段:

```python
class PetSchema(Schema):
    name = fields.String()
    animal_type = fields.String()

class PersonSchema(Schema):
    name = fields.String(dump_to = 'username')
    birthday = fields.Date()
    pets = fields.Nested(PetSchema,dump_to='pet',many=True)
```

由于这里，我们将用户的 name 属性修改为 username，因此我们需要在字段中使用`dump_to`参数将其修改为我们需要的字段。另外，用户的 pet 字段对应的是宠物的信息，因此我们采用嵌套模式来实现这样需求

## 序列化模型
上面我们已经定义好了我们的模式了,下一步是序列化模型的操作了

我们可以这样来操作:

```python
query=Person.select(Person, Pet).join(Pet)
```

接着,我们实例化我们的模式,然后传入需要序列化的模型:

```python
person, error = PersonSchema(many = True).dumps(query.aggregate_rows())
```

在这里，我们调用 PersonSchema 实例的 dumps 来生成 JSON 数据，另外它还有1个 dump 方法用于生成 Python 对象。由于我们的渲染的数据有多条，因此我们需要在实例化 PersonSchema 类时传入关键字参数 many 为 True，不然没有任何数据

通过这种方式，PersonSchema 会查看它自己的属性，将数据模型中对应的数据先序列化出来，然后是查询嵌套模式中的字段，如果符合对应的名称则将其序列化出来，最后我们将得到这样的数据:

```python
[
    {
        "username": "Bob",
        "pet": [
            {
                "animal_type": "cat",
                "name": "Kitty"
            }
        ],
        "birthday": "1960-01-15"
    },
    {
        "username": "Herb",
        "pet": [
            {
                "animal_type": "dog",
                "name": "Fido"
            },
            {
                "animal_type": "cat",
                "name": "Mittens"
            }
        ],
        "birthday": "1950-05-05"
    },
    {
        "username": "Grandma",
        "pet": [
            {
                "animal_type": "cat",
                "name": "Jack"
            }
        ],
        "birthday": "1935-03-01"
    }
]
```

可以看到,通过 marshallow 得到的结果与之前我们编写的序列化操作的结果是一样的

不得不说，marshallow 这个库对于序列化模型其实挺实用的。当然对于复杂的模型，我们需要利用合适的方式将其搜索出来，不然还是序列化不了的

[参考资料](https://marshmallow.readthedocs.io/en/latest/quickstart.html)