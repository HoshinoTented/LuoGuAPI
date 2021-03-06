[![Join the chat at https://gitter.im/LuoGuAPI/Lobby](https://badges.gitter.im/LuoGuAPI/Lobby.svg)](https://gitter.im/LuoGuAPI/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![](https://jitpack.io/v/HoshinoTented/LuoGuAPI/month.svg)][jitpack]
[![Jitpack](https://jitpack.io/v/HoshinoTented/LuoGuAPI.svg)][jitpack]
[![](https://img.shields.io/bintray/v/hoshinotented/hoshino9/luoguapi.svg)](https://bintray.com/hoshinotented/hoshino9/luoguapi)

# LuoGuAPI
[**你谷**](https://www.luogu.org) 的api

感谢您谷，终于美化 API 了  

# CI
CI      |Status
-------:|:---------
CircleCI|[![CircleCI](https://circleci.com/gh/HoshinoTented/LuoGuAPI.svg?style=svg)](https://circleci.com/gh/HoshinoTented/LuoGuAPI)
AppVeyor|[![Build status](https://ci.appveyor.com/api/projects/status/l66p8yqgxgjl9jph?svg=true)](https://ci.appveyor.com/project/HoshinoTented/luoguapi)

 [jitpack]: https://jitpack.io/#HoshinoTented/LuoGuAPI

# Usage

First, add `https://dl.bintray.com/hoshinotented/hoshino9` to your maven repository list.

Like this:

```groovy
repositories {
    maven { url = 'https://dl.bintray.com/hoshinotented/hoshino9' }
}
```

## Gradle

```groovy
compile 'org.hoshino9:luoguapi-[submodule name]:[version]'
```

```kotlin
compile("org.hoshino9:luoguapi-[module name]:[version]")
```

## Maven

```xml
<dependency>
  <groupId>org.hoshino9</groupId>
  <artifactId>[submodule name]</artifactId>
  <version>[versioon]</version>
  <type>pom</type>
</dependency>
```

# Build
您需要准备一个 [JDK](https://oracle.com) 并配置正确的 `JAVA_HOME`。

在项目根目录下执行以下命令:
```bash
./gradlew shadowJar
./gradlew sourcesJar
```
会在 `<module>/build/libs` 下生成三个 `.jar` 文件

其中:
* `<module name>-<version number>-all.jar` 为本体（附带依赖）  
* `<module name>-<version number>-sources.jar` 为源码  

# Stable API
洛谷已 **正式** 开放 API 的功能（未选中的代表 `LuoGuAPI` 还未支持）:  

- [x] 登录
- [ ] 注册
- [x] 两步验证
- [x] 题目列表
- [x] 题目内容
- [x] 提交代码
- [x] 图床
- [ ] 题目的操作
- [x] 比赛列表
- [x] 比赛内容
- [ ] 比赛的操作
- [x] 剪切板列表
- [x] 剪切板内容
- [x] 新建剪切板
- [x] 删除剪切板
- [x] 编辑剪切板

洛谷还 **未正式** 开放但已经在开发中的 API（未选中同上）:  

- [ ] 博客的系列操作（不确定，可能已经正式开放）

无法在 API 列表中寻找到的功能:

- [ ] 讨论版
- [ ] ~~试炼场~~