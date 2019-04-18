[![Join the chat at https://gitter.im/LuoGuAPI/Lobby](https://badges.gitter.im/LuoGuAPI/Lobby.svg)](https://gitter.im/LuoGuAPI/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
# LuoGuAPI
[**你谷**](https://www.luogu.org) 的api ~~\(然而大部分都是解析HTML\)~~  

# CI
CI      |Status
-------:|:---------
CircleCI|[![CircleCI](https://circleci.com/gh/HoshinoTented/LuoGuAPI.svg?style=svg)](https://circleci.com/gh/HoshinoTented/LuoGuAPI)
Jitpack |[![Jitpack](https://jitpack.io/v/HoshinoTented/LuoGuAPI.svg)](https://jitpack.io/#HoshinoTented/LuoGuAPI)  
AppVeyor|[![Build status](https://ci.appveyor.com/api/projects/status/l66p8yqgxgjl9jph?svg=true)](https://ci.appveyor.com/project/HoshinoTented/luoguapi)

# 如何使用
您可以在 `JVM` 平台上使用  
目前这个项目没有跨平台的打算(如编译到 JavaScript)  
~~没人用要跨什么平台QAQ~~

# How to use (Real)
您需要准备一个 [JDK](https://oracle.com) 并配置正确的 `JAVA_HOME`。

在项目根目录下执行以下命令:
```bash
./gradlew assemble
```
会在 `build/libs` 下生成三个 `.jar` 文件  
其中:
* `luogu-<version number>.jar` 为本体  
* `luogu-<version number>-dependencies.jar` 为运行时依赖  
* `luogu-<version number>-sources.jar` 为源码
