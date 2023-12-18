### perf节点

可以统计性能相关数据的节点

#### 属性

- uni: print
- child: 需要统计性能的子节点

#### demo1

循环执行1万次，查看性能数据

输入：

```
{
    "uni": "perf",
    "child": {
        "uni": "loop",
        "loopTimes": 10000,
        "child": {
            "uni": "mix",
            "json": {
                "times": "${loopIndex+1}"
            }
        }
    }
}

```

输出：

```
{
	"output":{
		"times":10000
	},
	"executeInfo":{
		"beginTime":1702910434624,
		"endTime":1702910434708,
		"beginTimeShow":"2023-12-18 22:40:34.624",
		"endTimeShow":"2023-12-18 22:40:34.708",
		"costTime":84,
		"total":10000,
		"tps":119047
	}
}

```

说明：
- output: 子节点输出内容
- executeInfo: 执行信息
  - beginTime: 开始时间
  - endTime: 结束时间
  - beginTimeShow: 开始时间格式化
  - endTimeShow: 结束时间格式化
  - costTime: 耗时
  - total: 执行次数
  - tps: TPS计算
