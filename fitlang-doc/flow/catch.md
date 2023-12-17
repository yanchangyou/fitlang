## catch节点

捕获异常

#### 属性

- uni: catch
- child: 必须只能两个子节点，第一个是try节点，第二个是catch节点

#### demo

输入：

```
{
    "uni": "catch",
    "child": [
        {
            "name": "try node",
            "uni": "mix"
        },
        {
            "name": "catch node",
            "uni": "echo"
        }
    ]
}

```

输出:

```
{
	"exception":"mix node json field is required!",
	"input":{
		
	}
}
```

说明：执行mix节点时，json字段是必填，抛异常，第二个阶段捕获并输出.

#### demo2

下面演示，捕获异常后流程继续执行

```
{
    "uni": "loop",
    "loopTimes": 2,
    "isBagsMode": true,
    "child": {
        "uni": "catch",
        "child": [
            {
                "name": "try node",
                "uni": "mix"
            },
            {
                "name": "catch node",
                "uni": "echo"
            }
        ]
    }
}
```

输出

```
{
	"list":[
		{
			"exception":"mix node json field is required!",
			"input":{
				
			}
		},
		{
			"exception":"mix node json field is required!",
			"input":{
				"exception":"mix node json field is required!",
				"input":{
					
				}
			}
		}
	]
}
```

从输出结果，可以看出循环执行了两次，每一次都抛了异常.