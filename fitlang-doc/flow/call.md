## call节点

调用其他节点执行

#### 属性
- uni: switch
- nodeId: 其他节点id

#### demo
输入：
```
{
    "uni": "sequence",
    "child": [
        {
            "id": "node1",
            "uni": "mix",
            "json": {
                "value": 1
            }
        },
        {
            "id": "node2",
            "uni": "mix",
            "json": {
                "value": 2
            }
        },
        {
            "uni": "call",
            "nodeId": "node1"
        }
    ]
}

```
返回结果：
```
{
	"value": 1
}
```
按照顺序执行，执行到第三个子节点，call第一个子节点，返回value=1