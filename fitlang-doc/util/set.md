### set节点

设置当前节点输出和上下文变量属性名和属性值

#### 属性

- uni: set
- key: 属性名
- value: 属性值

#### demo1

输入：

```
{
    "uni": "setGlobal",
    "key": "foo",
    "value": "bar"
}
```

输出：

```
{
	"foo":"bar"
}

```

说明：输出就是set的key=value

#### demo2

```
{
    "uni": "sequence",
    "child": [
        {
            "uni": "setGlobal",
            "key": "foo",
            "value": "bar"
        },
        {
            "uni": "mix",
            "json": {
                "value": "${foo}"
            }
        }
    ]
}
```

出参：

```
{
	"value":"bar"
}
```

说明：第一个子节点设置上下文变量，第二个子节点获取变量的值