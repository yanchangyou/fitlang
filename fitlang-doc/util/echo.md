### echo节点

把入参原样输出，就像echo的本意回声一样，方便知道入参是什么.
echo能执行，说明程序能够处理入参.

### 属性

- uni: echo

#### demo1

入参：

```
{
    "uni":"echo"
}
```

出参：

```
{
}
```

说明：没有入参时，出参也是为空

#### demo2

```
{
    "uni": "echo",
    "input": {
        "foo": "bar"
    }
}
```

出参：

```
{
	"foo":"bar"
}
```

说明：出参和入参一样