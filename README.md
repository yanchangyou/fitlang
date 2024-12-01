## 介绍

FitLang是面向开发人员群体的，适合做一个IDEA环境下小工具：postman、json比较、乱码探测、加密解密、进制转换、RPA、中间件，只要有IDEA，安装FitLang插件，添加对应的工具json代码，这些工具都有了

把日常开发工作中，遇到的json数据处理地方，沉淀到工具箱，方便后续使用，目前积累了近百个节点服务，适合DIY扩展

IDEA + 如意插件 = 可视化脚本语言 + 中间件 + 动态插件 + 迷你应用 + 迷你工具

支持节点组: json，flow，http，excel，telnet，clipboard，file，cmd，ide，info，test，mock，perf，monitor
最后有详细列表

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

plugin:
![](https://plugins.jetbrains.com/files/22593/screenshot_cc167984-8557-41da-8211-36eeb5864633)

script:
![](https://plugins.jetbrains.com/files/22593/screenshot_2ce2a61a-43b9-4569-bcc1-9d98e8b5f306)
![](https://plugins.jetbrains.com/files/22593/screenshot_573fe927-a2e3-4abf-b012-8c4a25029419)

server:
![](https://plugins.jetbrains.com/files/22593/screenshot_610c35af-8ae6-45cd-a102-bf3b90a74745)

applet:
![](https://plugins.jetbrains.com/files/22593/screenshot_f515df32-000b-4f4b-9cdd-676e2342666b)

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
- setGlobal: 设置全局变量
- print: 控制台打印
- log: IDE控制台输出
- sleep: 流程休眠节点
- perf: 耗时统计
- replaceContent: 文本内容查找替换

### json操作节点

- convert: 转换节点，支持json到json的转换，使用转换表达式
- removeField: 移除json字段
- removeEmptyField: 移除空字段
- mix: 混入json对象
- mixNode: 混入节点执行，用于嵌套字段场景
- eval: 表达式计算
- parseJson: 解析json
- stringifyJson: json转字符串
- convertObjectToArray: 对象转数组
- convertArrayToObject: 数组转换对象
- convertToObjectArray: 转换为对象数组
- convertToBasicArray: 转换为基本数组
- get: 使用json path获取值 （json path语法说明：https://alibaba.github.io/fastjson2/jsonpath_cn）
- set: 使用json path设置值
- getStruct: 获取json结构
- getSchema: 获取JsonSchema
- sortField: 按照字段排序（字母表升序）
- add: 支持json相加
- increase: 加+1
- decrease: 减-1
- compare: 比较json
- diff: 比较json，只返回差异

### 流程节点（有child子节点）

- sequence: 顺序执行节点
- batch: 批量执行节点
- pipe: 管道执行节点
- foreach: 遍历数组或对象执行
- loop: 循环执行节点，loopTimes制定执行次数
- switch: 分支执行节点，switchField指定分支字段
- return: 返回json
- thread: 多线程执行
- execute: 执行入参传递的流程
- call: 引用节点执行
- catch: 异常捕获，包含try节点和catch节点两个子节点
- assert: 断言
- node: 子流程

### 函数

- function: 函数
- package: 包

### http节点

- http: http
- postJson: http post json
- postForm: http post form
- httpGet: http get
- httpPut: http put
- httpDelete: http delete
- httpHead: http head
- postman: postman

### Web节点

- server: 服务端节点, 类属于tomcat
- proxy: 代理节点
- web: 配置web参数，响应头等

### file节点

- readFile: 读取文件
- writeFile: 写入文件
- deleteFile: 删除文件

### 系统信息节点

- systemInfo: 获取系统信息，基于oshi实现(信息字段(支持屏蔽)
  ：computerManufacturer,computerModel,processorName,processorPhysicalCount,processorLogicalCount,processorMaxFreq,memoryTotal,memoryAvailable,osManufacturer,osFamily,osVersion,osBit)
- info: 获取系统信息，基于hutool SystemUtil实现(信息字段(支持屏蔽)
  ：os,memory,jvm,host,runtime,javaSpec,jvm,user,properties,env)

### os节点

- getClipboard: 读取剪贴板内容
- setClipboard: 设置剪贴板内容

### 命令行

- cmd: 命令行
- zip: 压缩
- unzip: 解压

### 网络

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

### IDE节点

- readEditor: 获取当前编辑器内容
- writeEditor: 写入当前编辑器内容
- showConfig: 显示配置
- readConfig: 读取配置
- openWebPage: 打开web页面
- showHtml: open web page
  showJsonPage: show json page
- chooseFile: 选择文件
- showInfoMessage: 显示消息
- showWarningMessage: 显示警告信息
- showErrorMessage: 显示错误信息
- showInputDialog: 显示输入对话框
- showOkCancelDialog: 显示确认或取消对话框
- showGlobalConfigDialog: 显示全局配置对话框
- showYesNoCancelDialog: Yes, No, Cancel对话框
- showPasswordDialog: password对话框
- showCheckboxOkCancelDialog: 勾选对话框

### 办公软件节点

- readExcel: 读取Excel
- writeExcel: 写入Excel
- mergeExcel: 合并Excel
- readExcelForAllSheet: 读取所有sheet内容

### 小应用

- applet: 小应用

## 🔋 JetBrains开源授权

FitLang在JetBrains公司的免费开源授权下，通过IDEA IDE开发，在此表达我的感谢。

<a href="https://jb.gg/OpenSourceSupport" target="_blank"><img src="https://resources.jetbrains.com/storage/products/company/brand/logos/jb_beam.png" alt="JetBrains Logo (Main) logo." width="300"></a>
