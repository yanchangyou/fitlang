一个最简单的服务器demo，添加文件server.fit,输入一下内容：

```
{
    "uni": "server"
}
```

在IDEA中，右键执行，得到启动输出信息：

```
{"message":"server start OK!","httpPrefix":"http://127.0.0.1","port":11111,"url":"http://127.0.0.1:11111"}
```

服务器默认使用11111端口，访问路径：http://127.0.0.1:11111/hello

通过浏览器访问，返回如下内容：

```
{
    "welcome": "hello, fit server!",
    "server": [
        {
            "file": "/opt/github/fitlang/fitlang-server/demo/fitserver/app/first.fit",
            "url": "http://127.0.0.1:11111"
        }
    ],
    "service": [
        {
            "path": "/_api",
            "url": "http://127.0.0.1:11111/_api",
            "description": "show api menu"
        },
        {
            "path": "/_raw",
            "url": "http://127.0.0.1:11111/_raw",
            "description": "show raw content"
        },
        {
            "path": "/_shutdown",
            "url": "http://127.0.0.1:11111/_shutdown",
            "description": "stop this websocket server"
        },
        {
            "path": "/_stop",
            "url": "http://127.0.0.1:11111/_stop",
            "description": "stop this server"
        },
        {
            "path": "/_ip",
            "url": "http://127.0.0.1:11111/_ip",
            "description": "get client ip"
        },
        {
            "path": "/_reload",
            "url": "http://127.0.0.1:11111/_reload",
            "description": "reload this server"
        }
    ]
}
```

输出说明：

- welcome：欢迎字段，支持自定义
- server： 已经启动服务器，支持启动多个
    - file: 启动文件路径
    - url: 访问的url
    - port：启动端口，支持自定义
- service：当前端口启动服务列表，下换线开头的是内置的服务，一下常规的维护操作
    - _api: 所有的api服务清单
    - _raw: 原始文件内容，默认访问.fit结尾文件，会动态解析执行，通过_raw可以获取原始内容
    - _shutdown: 关闭服务，内部使用System.exit()，如果是本机执行，会关闭IDEA，谨慎操作
    - _stop: 关闭当前端口的服务，仅限本机操作
    - _reload: 重新加载
    - _ip: 获取服务器ip

### 更多配置

端口、欢迎字段、扩展服务配置如下：

```
{
    "uni": "server",
    "port": 11112,
    "welcome": "Welcome to FitLang world!",
    "service": {
        "/hello": {
            "uni": "hello"
        }, 
        "/execute": {
            "uni": "execute"
        }
    }
}
```

其中service用于配置路由服务，启动后，服务URL会多两个

- /hello: http://127.0.0.1:11112/hello 支持入参：?who=fit
- /execute: http://127.0.0.1:11112/execute
  把入参的内容当成fit代码执行

### 静态资源

如果启动目录有静index.html文件，默认根路径时，会展示index.html内容，和nginx类似

### 动态资源

如果访问以.fit结尾的资源，会动态执行后，返回结果，和tomcat的JSP类似
