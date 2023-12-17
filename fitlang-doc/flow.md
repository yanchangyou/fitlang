## 流程节点

- sequence: 顺序执行节点
- batch: 批量执行节点
- pipe: 管道执行节点
- foreach: 遍历json数组字段节点
- loop: 循环执行节点，loopTimes制定执行次数
- switch: 分支执行节点，switchField指定分支字段
- return: 返回json
- thread: 多线程执行
- execute: 执行入参传递的流程
- call: 引用节点执行

sequence和pipe的区别在于：
- sequence：所有子节点的入参一样
- pipe: 当前子节点的入参来源于上一个节点，没有上一个节点时，来源于父节点或外部入参

用demo演示差异
#### sequence
```
{
    "flag": "needFormatJsonInConsoleFlag",
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
sequence是

#### pipe
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

对比发现：一个是hello, world， 另外一个hello, fit; sequence不影响hello的入参，而pipe是影响的；
pipe就是管道的意思，如果管道里面是水，上一个节点变成了红色，下一个节点，接收到的也是红色.