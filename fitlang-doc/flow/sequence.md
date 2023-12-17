### sequence节点

顺序执行节点，按照顺序一步一步的执行.

#### 属性
- uni: sequence
- isBagsMode: 是否袋子模式，如果是，就把每个子节点的内容放入袋子中，否则不放入
- bagsName: 袋子的名字，子节点出参数组字段
- child: 子节点，按照顺序执行的子节点数组

一个简单的demo, 先mix，再hello，mix字段内容用于hello的message拼接

```
{
    "uni": "sequence",
    "child": [
        {
            "uni": "mix",
            "json": {
                "who": "Fit"
            }
        },
        {
            "uni": "hello"
        }
    ]
}
```
输出结果：
```
{
	"message":"hello, world!"
}
```
输出结果并没有体现mix的字段，是由于顺序执行，最终输出结果是最后一个子节点的输出，如果要看每个节点输出内容，就设置为袋子模式，demo如下：

```
{
    "uni": "sequence",
    "isBagsMode": true,
    "child": [
        {
            "uni": "mix",
            "json": {
                "who": "Fit"
            }
        },
        {
            "uni": "hello"
        }
    ]
}
```
输出结果：
```
{
	"list":[
		{
			"who":"Fit"
		},
		{
			"who":"Fit",
			"message":"hello, world!"
		}
	]
}
```
输出数组字段名默认是list，如果要改名名称，通过bagsName属性配置