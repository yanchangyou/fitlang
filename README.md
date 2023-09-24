## 介绍

fit语言是一门轻量级编程语言，兼具开发人员友好和工具处理友好。

使用AST+JSON来编写代码，并且JSON是唯一的数据类型，插件式设计方便扩展节点功能。

第一个demo: hello.fit

内容

```
{
    "uni": "hello",
    "who": "world"
}

```
### 说明
- hello.fit 后缀以fit结尾，语言插件继承IDEA自带的JSON语言，使用JSON的语法校验、高亮、格式等
- uni 统一节点描述符(借鉴URI)，区分不同的处理，内部对应一个实现类
- input: 入参json

最后有更多节点列表

### 执行
编辑器里面，点击右键，选择【Run FitLang】执行，代码执行完将输出

```
{"message":"hello, world!"}
```

## 插件

[IDEA插件](https://plugins.jetbrains.com/plugin/22593-fitlang/versions)

插件审核通过后，遇到.fit结尾文件时会提示下载，预览版需要下载zip包，然后在IDEA中按照，使用方式如下

[预览版插件安装](https://plugins.jetbrains.com/plugin/22593-fitlang/plugin)

![](https://plugins.jetbrains.com/files/22593/screenshot_cc167984-8557-41da-8211-36eeb5864633)

## demo
- [demo-hello](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-hello)
- [demo-mock](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-mock)
- [demo-server](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-server)
- [github-all-demo](https://github.com/yanchangyou/fitlang-demo)

## 应用场景
- mockServer: mock json返回
- 代理透传
- 静态文件服务器
- 微服务
- http服务调用
- 自动化测试
- 服务器简单监控

## 中间件
FitServer 使用fit语言开发的中间件，相当于轻量级的Nginx，tomcat，SpringBoot，网站部署在FitServer上: 
[http://fit.321zou.com](http://fit.321zou.com)

## 后续计划
- 封装更多Hutool工具
- 性能测试

## 节点列表

### 实用节点
- hello: hello world demo
- echo: 原样返回入参
- add: 支持json相加
- convert: 转换节点，支持json到json的转换，使用转换表达式
- removeField: 移除json字段
- removeEmptyField: 移除空字段
- print: 控制台打印
- sleep: 流程休眠节点
- replace: 替换结果输出
- mix: 混入json对象
- mixNode: 混入节点执行，用于嵌套字段场景
- execute: 执行入参传递的流程
- assert: 断言
- eval: 表达式计算
- set: 设置全局变量

### 流程节点（有child子节点）
- sequence: 顺序执行节点
- pipe: 管道执行节点
- foreach: 遍历json数组字段节点
- loop: 循环执行节点，loopTimes制定执行次数
- switch: 分支执行节点，switchField指定分支字段
- return: 返回json
- thread: 多线程执行

### Web节点
- postJson: http post json
- postForm: http post form
- httpGet: http get
- httpPut: http put
- httpDelete: http delete
- server: 服务端节点, 类属于tomcat
- proxy: 代理节点
- web: 配置web参数，响应头等
- wsServer: web socket server
- wsClient: web socket client

### Excel节点
- readExcel: 读取Excel
- writeExcel: 写入Excel

### 系统信息节点
- systemInfo: 获取系统信息(暂时开放字段：computerManufacturer,computerModel,processorName,processorPhysicalCount,processorLogicalCount,processorMaxFreq,memoryTotal,memoryAvailable,osManufacturer,osFamily,osVersion,osBit)

### 监控节点
- startMonitor: 启动监控
- getMonitorData: 获取监控数据: CPU和Memory
- getClientMonitorData: 获取单个客户端监控数据
- receiveClientMonitorData: 接收客户端监控数据
- pushClientMonitorData: 推送监控数据
- getMonitorClient: 获取监控客户端列表

### 云节点
- cloudServer: server端
- cloudClient: client端
