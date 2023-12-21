### assert节点

断言节点

#### 属性

- uni: assert
- needToString: 是否需要把结果转换为字符串，然后比较
- expected: 期望值
- containField: 包含字段
- containJson: 是否包含json

#### demo1

输入：

```
{
    "input": {
       "hello": "world" 
    },
    "uni": "assert",
    "needToString": true,
    "expected": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":true
}
```
说明：断言成功，把入参原样透传


#### demo2

输入：

```
{
    "input": {
       "foo": "bar" 
    },
    "uni": "assert",
    "needToString": true,
    "expected": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":false,
	"actual":{
		"foo":"bar"
	},
	"expected":{
		"hello":"world"
	},
	"input":{
		"foo":"bar"
	}
}
```
说明：断言失败，把入参、期望和实际都输出


#### demo3

输入：

```
{
    "input": {
       "foo": "bar" 
    },
    "uni": "assert",
    "needToString": true,
    "expected": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":true
}
```
说明：断言失败，把入参、期望和实际都输出


#### demo4

输入：

```
{
    "input": {
       "foo": "bar" 
    },
    "uni": "assert",
    "needToString": true,
    "expected": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":true
}
```
说明：断言成功，把入参原样透传

#### demo5

输入：

```
{
    "input": {
       "foo": "bar" 
    },
    "uni": "assert",
    "needToString": true,
    "expected": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":false,
	"actual":{
		"hello":"world"
	},
	"expected":"contain fields: [\"foo\"]",
	"input":{
		"hello":"world"
	}
}
```
说明：断言失败，把入参、期望和实际都输出



#### demo6

输入：

```
{
    "input": {
        "foo": "bar"
    },
    "uni": "assert",
    "containJson": {
        "hello": "world"
    }
}
```

输出：

```
{
	"success":false,
	"actual":{
		"foo":"bar"
	},
	"expected":"{\"hello\":\"world\"}",
	"input":{
		"foo":"bar"
	}
}

```
说明：断言成功，把入参原样透传
