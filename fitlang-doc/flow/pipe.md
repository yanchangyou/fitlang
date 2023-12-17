## pipe节点

以管道方式执行，上一个节点的出参就是下一个阶段的入参，管道的特点很形象体现这一特征，属性如下：
- uni: pipe
- child: 子节点，按照顺序执行的子节点数组

一个简单的demo, 先mix，再hello，mix字段内容用于hello的message拼接

```
{
    "flag": "needFormatJsonInConsoleFlag",
    "uni": "pipe",
    "child": [
        {
            "uni": "mix",
            "json": {
                "who": "fit"
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
	"message":"hello, fit!"
}
```
输出的结果包含了fit字段, 这个就是和sequence执行的区别