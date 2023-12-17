## execute节点

执行入参里面的脚本

#### 属性
- uni: thread

#### demo
输入：
```
{
    "input": {
        "uni": "hello"
    },
    "uni": "execute"
}
```
返回结果：
```
{
	"message":"hello, world!"
}
```
input字段里面是脚本内容