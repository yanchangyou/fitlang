## 介绍

fit语言是一门轻量级编程语言，兼具开发人员友好和工具处理友好。

特点：
- 使用AST思路设计和编写代码
- 入参和出参都是JSON
- 通过插件扩展功能，对应代码中的一个个节点（把编程语言的关键字做成编排节点）
- 中间件，类似Nginx和Tomcat
- 支持IDEA静态插件和动态插件
- 支持postman转换
- 内置实现了大量节点功能

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

完整节点清单见最后节点章节

### 执行
编辑器里面，点击右键，选择【Run FitLang】执行，代码执行完将输出

```
{"message":"hello, world!"}
```

## 插件

[IDEA插件](https://plugins.jetbrains.com/plugin/22593-fitlang/versions)

插件审核通过后，遇到.fit结尾文件时会提示下载，预览版需要下载zip包，然后在IDEA中按照，使用方式如下

![](https://plugins.jetbrains.com/files/22593/screenshot_cc167984-8557-41da-8211-36eeb5864633)
![](https://plugins.jetbrains.com/files/22593/screenshot_2ce2a61a-43b9-4569-bcc1-9d98e8b5f306)
![](https://plugins.jetbrains.com/files/22593/screenshot_573fe927-a2e3-4abf-b012-8c4a25029419)
![](https://plugins.jetbrains.com/files/22593/screenshot_610c35af-8ae6-45cd-a102-bf3b90a74745)

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
- 数据库
- redis
- 消息中间件
- 封装更多Hutool工具
- 性能测试

## 节点列表

### 实用节点
- hello: hello world demo
- echo: 原样返回入参
- assert: 断言
- set: 设置全局变量
- sleep: 流程休眠节点
- print: 控制台打印
- perf: 耗时统计

### json数据节点
- add: 支持json相加
- convert: 转换节点，支持json到json的转换，使用转换表达式
- removeField: 移除json字段
- removeEmptyField: 移除空字段
- mix: 混入json对象
- mixNode: 混入节点执行，用于嵌套字段场景
- eval: 表达式计算
- parseJson: 解析json
- stringifyJson: json转字符串
- convertKeyValueList: key value list转换对象

### 流程节点（有child子节点）
- sequence: 顺序执行节点
- pipe: 管道执行节点
- foreach: 遍历json数组字段节点
- loop: 循环执行节点，loopTimes制定执行次数
- switch: 分支执行节点，switchField指定分支字段
- return: 返回json
- thread: 多线程执行
- execute: 执行入参传递的流程
- call: 引用节点执行

### http节点
- http: http
- postJson: http post json
- postForm: http post form
- httpGet: http get
- httpPut: http put
- httpDelete: http delete

### Web节点
- server: 服务端节点, 类属于tomcat
- proxy: 代理节点
- web: 配置web参数，响应头等

### file节点
- readFile: 读取文件
- writeFile: 写入文件
- deleteFile: 删除文件

### 系统信息节点
- systemInfo: 获取系统信息，基于oshi实现(信息字段(支持屏蔽)：computerManufacturer,computerModel,processorName,processorPhysicalCount,processorLogicalCount,processorMaxFreq,memoryTotal,memoryAvailable,osManufacturer,osFamily,osVersion,osBit)
- info: 获取系统信息，基于hutool SystemUtil实现(信息字段(支持屏蔽)：os,memory,jvm,host,runtime,javaSpec,jvm,user,properties)

### 命令行
- cmd: 命令行
- telnet: telnet
- telnets: telnets
- telnet.http: telnet.http
- telnet.https: telnet.https

### 监控节点
- startMonitor: 启动监控
- getMonitorData: 获取监控数据: CPU和Memory
- getClientMonitorData: 获取单个客户端监控数据
- receiveClientMonitorData: 接收客户端监控数据
- pushClientMonitorData: 推送监控数据
- getMonitorClient: 获取监控客户端列表
