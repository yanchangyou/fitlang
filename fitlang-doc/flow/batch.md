## batch节点

批量方式执行，和sequence的袋子模式一样，由于sequence需要添加额外字段配置，就另外单独添加一个节点，方便直接使用

子节点是独立执行，并且需要查看每个节点的执行结果，就选用batch

#### 属性

- uni: batch
- child: 子节点，按照顺序执行的子节点数组

#### demo

输入：

```
{
    "uni": "batch",
    "child": [
        {
            "uni": "mix",
            "json": {
                "value": 1
            }
        },
        {
            "uni": "mix",
            "json": {
                "value": 2
            }
        }
    ]
}
```

输出:

```
{
	"list":[
		{
			"value":1
		},
		{
			"value":2
		}
	]
}
```