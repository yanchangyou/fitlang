### postman节点

基本实现了postman类似功能，调用方式如下：

```
{
    "uni": "postman",
    "method": "POST",
    "url": "http://fit.321zou.com/echo",
    "header": {
        "contentType": "application/json"
    },
    "query": {
        "foo": "bar"
    },
    "body": {
        "hello": "world"
    },
    "proxy": {
        "host": "",
        "port": 0
    }
}
```

属性说明，基本参考postman图形界面属性元素

- uni: postman节点
- method: 请求方法，支持：GET,POST,PUT,HEAD,DELETE
- url: 请求URL地址
- header: 请求头配置
- query： query参数
- body: body参数
- proxy: 代理配置

执行结果

```
{
	"url":"http://fit.321zou.com/echo?foo=bar",
	"host":"fit.321zou.com",
	"port":80,
	"status":200,
	"header":{
		"Server":"nginx/1.20.1",
		"Connection":"keep-alive",
		"Content-Length":"29",
		"Date":"Sat, 16 Dec 2023 14:44:00 GMT",
		"Content-Type":"application/json"
	},
	"cookie":[
	],
	"sizeInfo":{
		"header":154,
		"body":29
	},
	"size":183,
	"time":"127ms",
	"body":{
		"hello":"world",
		"foo":"bar"
	}
}
```

出参也是参考postman出参元素，常规信息都有

### postman转换

内部通过fit代码实现了，把postman导出文件转换为fit文件功能，在默认的项目插件定义有已经集成，参考路径

fitlang-project-plugin/plugins/default/plugin.fit

实现主要步骤：

- 设置postman导出文件路径，默认使用当前选择文件
- 读取文件内容
- 解析成json对象
- 转换字段
- 遍历postman中的request
- 转换字段，成fit格式
- 生成fit文件或目录

实现源码（一个较复杂的fit应用案例）

```
{
    "uni": "pipe",
    "child": [
        {
            "uni": "mix",
            "json": {
                "filePath": "${filePath}"
            }
        },
        {
            "uni": "readFile"
        },
        {
            "uni": "parseJson",
            "jsonField": "content"
        },
        {
            "uni": "convert",
            "express": {
                "item": "${content.item}",
                "collectionName": "${content.info.name}"
            }
        },
        {
            "uni": "set",
            "key": "collectionName",
            "value": "${collectionName}"
        },
        {
            "uni": "foreach",
            "foreachField": "item",
            "child": {
                "id": "switch",
                "uni": "switch",
                "switchField": "${this.?item!=null}",
                "child": [
                    {
                        "case": "false",
                        "uni": "pipe",
                        "child": [
                            {
                                "uni": "mix",
                                "json": {
                                    "isFolder": false
                                }
                            },
                            {
                                "id": "convertItem",
                                "uni": "pipe",
                                "child": [
                                    {
                                        "uni": "set",
                                        "key": "contentType",
                                        "value": "${this.?request.?body.?urlencoded!=null?'application/x-www-form-urlencoded':''}"
                                    },
                                    {
                                        "uni": "convert",
                                        "express": {
                                            "name": "${name}",
                                            "content.name": "${name}",
                                            "content.uni": "postman",
                                            "content.method": "${request.method}",
                                            "content.header": "${request.header}",
                                            "content.header.contentType": "${contentType}",
                                            "content.url": "${request.url.raw}",
                                            "content.query": "${request.body.urlencoded}",
                                            "content.body": "${request.body.raw}",
                                            "isFolder": "${isFolder}",
                                            "contentType": "${contentType}"
                                        }
                                    },
                                    {
                                        "uni": "convertKeyValueList",
                                        "listField": "content.query",
                                        "keyField": "key",
                                        "valueField": "value"
                                    },
                                    {
                                        "uni": "convertKeyValueList",
                                        "listField": "content.header",
                                        "keyField": "key",
                                        "valueField": "value"
                                    },
                                    {
                                        "uni": "convert",
                                        "isMixMode": true,
                                        "express": {
                                            "content.header.contentType": "${contentType}"
                                        }
                                    },
                                    {
                                        "uni": "mix",
                                        "json": {
                                            "filePath": "${fileDir + '/' +collectionName + (isFolder?parentName:'') + '/' + name+'.fit'}"
                                        }
                                    },
                                    {
                                        "uni": "parseJson",
                                        "jsonField": "content.query"
                                    },
                                    {
                                        "uni": "parseJson",
                                        "jsonField": "content.body"
                                    },
                                    {
                                        "uni": "writeFile",
                                        "format": true
                                    }
                                ]
                            }
                        ]
                    },
                    {
                        "case": "true",
                        "uni": "pipe",
                        "child": [
                            {
                                "uni": "mix",
                                "json": {
                                    "parentName": "${'/'+name}",
                                    "isFolder": true
                                }
                            },
                            {
                                "uni": "foreach",
                                "foreachField": "item",
                                "mixToItemField": [
                                    "parentName",
                                    "isFolder"
                                ],
                                "child": {
                                    "uni": "call",
                                    "nodeId": "convertItem"
                                }
                            }
                        ]
                    }
                ]
            }
        }
    ]
}

```
