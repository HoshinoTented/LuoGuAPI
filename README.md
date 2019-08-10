[![Join the chat at https://gitter.im/LuoGuAPI/Lobby](https://badges.gitter.im/LuoGuAPI/Lobby.svg)](https://gitter.im/LuoGuAPI/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
# Tips
[#52](https://github.com/HoshinoTented/LuoGuAPI/issues/52)  
本项目不再添加新功能，仅维护现有功能，直到洛谷开放api，就 archive 此项目  
如果洛谷表明短时间内不会开放api，则此项目将继续添加新功能  

# LuoGuAPI
[**你谷**](https://www.luogu.org) 的api ~~\(然而大部分都是解析HTML\)~~  
感谢您谷，终于美化 API 了  

# CI
CI      |Status
-------:|:---------
CircleCI|[![CircleCI](https://circleci.com/gh/HoshinoTented/LuoGuAPI.svg?style=svg)](https://circleci.com/gh/HoshinoTented/LuoGuAPI)
Jitpack |[![Jitpack](https://jitpack.io/v/HoshinoTented/LuoGuAPI.svg)](https://jitpack.io/#HoshinoTented/LuoGuAPI)  
AppVeyor|[![Build status](https://ci.appveyor.com/api/projects/status/l66p8yqgxgjl9jph?svg=true)](https://ci.appveyor.com/project/HoshinoTented/luoguapi)

# How to use (Real)
您需要准备一个 [JDK](https://oracle.com) 并配置正确的 `JAVA_HOME`。

在项目根目录下执行以下命令:
```bash
./gradlew assemble
```
会在 `<module>/build/libs` 下生成三个 `.jar` 文件  
其中:
* `<module name>-<version number>.jar` 为本体  
* `<module name>-<version number>-sources.jar` 为源码

在 `luogu/build/libs` 下还会有  
* `luogu-<version number>-dependencies.jar` 为运行时依赖  
