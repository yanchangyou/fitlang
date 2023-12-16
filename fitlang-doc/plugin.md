### 静态插件

为了方便执行fit代码，开发了IDEA插件，插件下载和安装如下：

从插件市场下载插件

- 路径：菜单：Settings -> Plugins -> Marketplace
- 快捷键：Cmd + ,

输入fitlang，绿色背景，大写白色F，小写it，就是此插件

或者新建，hello.fit文件，IDEA的智能感知，会提示下载插件，在提升操作即可

安装插件后（一般不需要重启），在文本编辑器或文件目录右键，会出现Fit的菜单选项，点击菜单，就会执行当前内容或文件

#### 智能提示
当在花括号后面输入内置实现节点名称前缀时，会以模板下拉列表方式，实现智能补全，提升开发效率

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

### 动态插件原理
通过IDEA的AnAction实现，见插件文档：
https://plugins.jetbrains.com/docs/intellij/basic-action-system.html

实现源码：
- 一级菜单实现：fitlang-plugin-idea/src/main/java/my/lang/action/FitLangPluginActionGroup.java
- 二级菜单实现：fitlang-plugin-idea/src/main/java/my/lang/action/FitLangPluginAction.java

一级菜单默认内置了10个实现类，编号0~9,如果插件定义文件中，有对应编号，并且有script或actions，就会显示，否则就隐藏，通过这种方式一级菜单

二级菜单：通过菜单组实现，定义多少，就加入多少，无数量限制

一级菜单：如果有script属性，只有一级菜单，无二级菜单；如果有actions属性，就有二级菜单；
通过代码配置
```
    //一级菜单判断是否有点击行为
    public boolean canBePerformed(@NotNull DataContext context) {
        return actionScript != null;
    }
    //update方法中，配置是否显示和生效，以及内容
    public void update(AnActionEvent event) {
        //...
        event.getPresentation().setEnabledAndVisible(true);
        event.getPresentation().setText(title);
        //...
    }
    //actionPerformed中行为定义
    public void actionPerformed(@NotNull AnActionEvent e) {
        //...
    }

    //二级菜单获取
    @Override
    public AnAction[] getChildren(AnActionEvent event) {
        return children;
    }

```

二级菜单扩展AnAction实现，添加到菜单组中的children中：
```
    actionList.add(new FitLangPluginAction(name, title, script));
```