### loop节点

支持循环N次执行，用于需要重复执行的场景

#### 属性

- loopTimes: 循环次数
- isPipe: 是否pipe方式执行，默认是sequence方式执行
- isBagsMode: 是否袋子模式，如果是，会返回每次执行结果，否则只返回最后一次执行
- bagsName: 袋子名称
- bagsStep: 袋子放入步长，相当于采样放入，避免数量太多，比如在性能测试场景，我们只需要采样部分数据放入
- parallelism: 并行度，支持并行执行，相当于线程数，但是并行执行会导致上下文中，比如index获取的是相同的值，需要判断是否有影响

#### demo

循环10次执行，输入

```
{
    "uni": "loop",
    "loopTimes": 10,
    "child": {
        "uni": "mix",
        "json": {
            "index": "${loopIndex}"
        }
    }
}
```

输出

```
{
	"index":9
}
```

#### 步长demo
输入
```
{
    "uni": "loop",
    "loopTimes": 10,
    "isBagsMode": true,
    "bagsStep": 2,
    "child": {
        "uni": "mix",
        "json": {
            "index": "${loopIndex}"
        }
    }
}
```
输出结果里面只有偶数
```
{
	"list":[
		{
			"index":0
		},
		{
			"index":2
		},
		{
			"index":4
		},
		{
			"index":6
		},
		{
			"index":8
		}
	]
}
```

##### 并行demo
并发度5，5个线程启动执行，输入如下：
```
{
    "uni": "loop",
    "loopTimes": 10,
    "isBagsMode": true,
    "bagsName": "list",
    "parallelism": 5,
    "child": {
        "uni": "mix",
        "json": {
            "index": "${loopIndex}"
        }
    }
}
```
输出结果的index，就不是按照顺序递增，内部获取index和index+1，不是串行执行，会有相同的值
```
{
	"list":[
		{
			"index":0
		},
		{
			"index":1
		},
		{
			"index":2
		},
		{
			"index":3
		},
		{
			"index":4
		},
		{
			"index":6
		},
		{
			"index":8
		},
		{
			"index":9
		},
		{
			"index":6
		},
		{
			"index":7
		}
	]
}
```
