## return节点

中断流程执行，返回结果

#### 属性
- uni: switch
- json: 返回的json对象

#### demo
下面演示，遇到return节点时，直接返回，不执行后续节点
```
{
    "uni": "sequence",
    "child": [
        {
            "uni": "return",
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
返回结果：
```
{
	"who":"fit"
}
```