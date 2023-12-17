### switch节点

在需要根据条件进行分支处理场景，使用switch节点.

#### 属性

- uni: switch
- switchField: 分支字段，支持表达式
- child: 子节点数组
    - case: 分支值

#### 简单demo

下面demo实现如下功能

- 当输入type=1时，输出 value=A
- 当输入type=2时，输出 value=B

如下输入：
```
{
    "input": {
        "type": "1"
    },
    "uni": "switch",
    "switchField": "type",
    "child": [
        {
            "case": "1",
            "uni": "mix",
            "json": {
                "value": "A"
            }
        },
        {
            "case": "2",
            "uni": "mix",
            "json": {
                "value": "B"
            }
        }
    ]
}
```

输出

```
{
	"type":"1",
	"value":"A"
}
```
