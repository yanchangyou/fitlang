### foreach节点

当需要遍历数组中没一个元素时，使用foreach，在child中配置处理逻辑

#### 属性
- uni: foreach
- foreachField: 遍历数组
- isPipe: 是否管道执行，默认是sequence方式执行
- indexName: 索引字段名称，放到上下文中，可以通过mix等方式获取
- child: 字节点，支持数组

下面demo展示使用
```
{
    "input": {
        "list": [
            "a",
            "b"
        ]
    },
    "uni": "foreach",
    "foreachField": "list",
    "child": {
        "uni": "mix",
        "json": {
        }
    }
}
```
输出结果：
```
{
	"list":[
		{
			"value":"a",
			"index":0
		},
		{
			"value":"b",
			"index":1
		}
	]
}
```
输出字段说明：
- list: 是输出数组字段，实现foreachField配置的字段名称
  - value: 遍历当前值，如果是基本数组类型，会转换为对象数组，整体执行引擎内部入参和出参都是json对象
  - index: 元素所在的位置

### foreach 对象数组

```
{
    "input": {
        "list": [
            {
                "code": "1001"
            },
            {
                "code": "1002"
            }
        ]
    },
    "uni": "foreach",
    "foreachField": "list",
    "indexName": "loopIndex",
    "child": {
        "uni": "mix",
        "json": {
            "loopIndex": "${loopIndex}"
        }
    }
}
```
输出结果：
```
{
	"list":[
		{
			"code":"1001",
			"loopIndex":0
		},
		{
			"code":"1002",
			"loopIndex":1
		}
	]
}
```
### foreach 对象

输入：
```
{
    "flag": "needFormatJsonInConsoleFlag",
    "input": {
        "object": {
            "code": "1001",
            "foo": "bar"
        }
    },
    "uni": "foreach",
    "foreachField": "object",
    "child": {
        "uni": "echo"
    }
}
```
输出：
```
{
	"object":[
		{
			"key":"code",
			"value":"1001"
		},
		{
			"key":"foo",
			"value":"bar"
		}
	]
}
```