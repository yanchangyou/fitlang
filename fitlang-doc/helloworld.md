### hello world
从HelloWorld开始

新建文件：hello.fit，使用JSON的语法，编辑如下内容：
```
{
    "input": {
        "who": "world"
    },
    "uni": "hello"
}
```
input字段是入参，对谁hello，执行完之后输出:
```
{"message":"hello, world!"}
```

还可以更简单，默认是：world，输出结果也一样

```
{
    "uni": "hello"
}
```

也支持把who字段放到，节点定义字段上
```
{
    "uni": "hello",
    "who": "world"
}
```

表达式支持（使用MVEL）

```
{
    "uni": "hello",
    "who": "fit",
    "message": "${'hello, '+who+'!'}"
}
```

### 取值顺序
变量取值顺序：入参、节点属性、上下文

### 格式化
默认输出时没有格式化的，如果要在控制台格式化输出，只需要添加属性：
```
{
    "flag": "needFormatJsonInConsoleFlag",
    "uni": "hello"
}
```
输出结果就是格式化的JSON，方便查看结果
```
{
	"message":"hello, world!"
}
```