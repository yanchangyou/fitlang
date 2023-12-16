### 静态插件

为了方便执行fit代码，开发了IDEA插件，插件下载和安装如下：

从插件市场下载插件

- 路径：菜单：Settings -> Plugins -> Marketplace
- 快捷键：Cmd + ,

输入fitlang，绿色背景的, 大写的白色F，小写的it，就次插件

或者新建，hello.fit文件，IDEA的智能感知，会提示下载插件，在提升操作即可

安装插件后（一般不需要重启），在文本编辑器或文件目录右键，会出现Fit的菜单选项，点击菜单，就会执行当前内容或文件

### 动态插件

同时fit支持IDEA的动态插件开发，鼠标右键时，根据配置动态扩展菜单项，配置如下

在当前项目的根路径下面新建文件: plugin.fit，内容如下：

```
{
    "debug": true,
    "groups": [
        {
            "name": "FitActionGroup0",
            "title": "Fit Hello",
            "script": {
                "uni": "hello",
                "who": "fit"
            }
        },
        {
            "name": "FitActionGroup1",
            "title": "Fit Menu2",
            "actions": [
                {
                    "name": "zip",
                    "title": "Zip",
                    "script": {
                        "uni": "zip"
                    }
                }
            ]
        }
    ]
}
```

添加后，鼠标右键，就会出现一个新的菜单选项：Fit Hello

配置项说明

- debug: debug为true时，修改配置就会实时生效
- groups: 菜单组，支持两级菜单
    - name: 菜单名称，必须使用内置的名称：FitActionGroup0-9,内置了10个一级菜单（不能重复）
    - title: 菜单显示名称
    - script: 点击菜单后，会执行此处定义的脚步内容，也是fit语法
    - actions: 二级菜单定义，不限数量（script和actions只使用一个，优先script配置）
        - name: 二级菜单内部名称（自定义，不能重复）
        - title: 二级菜单显示名称
        - script：二级菜单执行内容

### 动态插件案例

动态插件放到目录中：/fitlang-project-plugin/plugins

开源项目go-doudou配置如下，支持项目代码生成、启动等相关操作，简化命令行操作

案例：/fitlang-project-plugin/plugins/godoudou/plugin.fit

放到GoLand的IDE项目根路径下，就会显示动态菜单