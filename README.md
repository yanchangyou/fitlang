## 介绍

fit语言是一门低代码编程语言，兼具开发人员友好和工具处理友好。

使用AST+JSON来编写代码，并且JSON是唯一的数据类型，插件式设计方便扩展节点功能。

第一个demo：hello.fit

内容

```
{
    "input": {
        "who": "world"
    },
    "flow": {
        "uni": "hello"
    }
}

```

### 说明

- hello.fit 后缀以fit结尾，语言插件继承IDEA自带的JSON语言，使用JSON的语法校验、高亮、格式等
- input：入参定义
- flow：处理流程定义
- uni 统一节点描述符(借鉴URI)，区分不同的处理，内部对应一个实现类

## 执行

编辑器里面，点击右键，选择【Run FitLang】执行，代码执行完将输出

`{"message":"hello, world!"}`

### 节点列表

实用节点

- hello: hello world demo
- echo: 原样返回入参
- add：支持json相加
- convert：转换节点，支持json到json的转换，使用转换表达式
- removeField：移除json字段
- removeEmptyField：移除空字段
- print：控制台打印
- sleep：流程休眠节点

流程节点（有child子节点）

- sequence：顺序执行节点
- pipe：管道执行节点
- foreach：遍历json数组字段节点
- loop：循环执行节点，looptTimes制定执行次数
- switch：分支执行节点，switchField指定分支字段

工具节点 （封装hutool）

- http: 实现http请求
- server: 封装http server服务
- proxy: 实现代理服务
- fileServer: 实现简单文件服务器

## 应用场景

- mockServer：mock json返回
- 代理透传
- 静态文件服务器
- 微服务
- http服务调用

## 插件

插件审核通过后，遇到.fit结尾文件时会提示下载，预览版需要下载zip包，然后在IDEA中按照，使用方式如下

[更多插件相关](https://plugins.jetbrains.com/plugin/22593-fitlang/plugin)

![](https://plugins.jetbrains.com/files/22593/58337-page/fbf57e79-c760-4055-8252-7e47adedb068)

## demo

- [demo-hello](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-hello)
- [demo-mock](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-mock)
- [demo-server](https://plugins.jetbrains.com/plugin/22593-fitlang/demo-server)
- [github-all-demo](https://github.com/yanchangyou/fitlang-demo)

## 后续计划

- 封装更多hutool工具
- 自动化测试
- 性能测试
