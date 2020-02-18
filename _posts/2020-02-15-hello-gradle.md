---
layout: post
title: 你好，Gradle！
post: YES!
---

**前置知识：Java**

Gradle 是一种构建工具，可以通过编写脚本来做到依赖管理等等……

目前 Gradle 的脚本所支持的语言有 Groovy 和 Kotlin。

> Groovy 支持大部分 Java 语法，所以完全可以把 Groovy 当做 Java 用

## Gradle 能做什么

* 管理依赖
* 一键部署
* 自动生成代码
* 等等……

Gradle 是一个很常用的工具，一般用于 JVM 开发，但是 Gradle 提供了插件系统，你可以开发自己的 Gradle 插件来支持其他你需要的语言。

## 运行一个 Java 程序

既然 Gradle 是一个构建工具，那应当能构建一个普通的项目。

首先通过 `gradle init` 来创建一个普通的 Gradle 项目，会出现一些选项，这里我们选择：2-3-1-4-<回车>-<回车>

之后会创建一个类似以下类型的项目：

```plain
root
  - gradle/                 // Gradle Wrapper 目录
  - src/                    // 源代码目录
      - main/               // 应用代码目录
          - java/           // Java 代码目录
          - resources/      // 应用资源目录
      - test/               // 测试代码目录
          - java/           // Java 测试代码目录
          - resources/      // 测试资源目录
  - build.gradle            // 项目脚本文件
  - settings.gradle         // 项目配置文件
  - gradlew                 // gradlew 文件，用于启动项目级别的 Gradle
  - gradlew.bat             // 同上，用于 Windows 平台
```

最重要的是 `build.gradle` 文件，来看看里面有什么：

```groovy
plugins {
    id 'java'
    id 'application'
}

repositories {
    jcenter()
}

dependencies {
    testImplementation 'junit:junit:4.12'
}

application {
    mainClassName = 'test.App'
}
```

首先是 `plugins { ... }`，这个是模块的插件配置代码，比如 `id 'java'` 就是应用了 `java` 插件。

接着是 `repositories { ... }` 块，用于添加远端的依赖源，比如 `jcenter` 或者是 `mavenCentral()`。

接下来是 `dependencies { ... }` 块，用于添加模块的依赖，会从上面配置的依赖源中查找依赖并自动下载。

最后是 `application { ... }` 块，这是 `application` 插件的配置代码，`mainClassName` 字段代表了主类的全限定名，让 `application` 知道该从哪里启动程序。

说了这么多，该说说怎么启动这个程序了：

```bash
./gradlew run
```

`run` 是 `application` 添加的一个 **任务（Task）**，`application { ... }` 块中的配置会引导这个任务找到主类，然后启动它：

```plain
> Task :run
Hello world.

BUILD SUCCESSFUL in 7s
2 actionable tasks: 2 executed
```

发现输出了 “Hello world.”，我们成功地启动这个程序了！

## 管理依赖

在实际开发中，通常需要很多第三方库 ~~来避免造轮子~~，但是去网上找 jar 然后下载下来太麻烦了，怎么办呢？

答案是：使用 Gradle。

假如我们需要 Gson 库（Google 开发的 JSON 解析库），于是可以在 `build.gradle` 的 `dependencies { ... }` 块中添加如下代码：

```groovy
dependencies {
    compile 'com.google.code.gson:gson:2.8.6'
}
```

如果你曾经用过 Maven，那么这段代码与 Maven 中的：

```xml
<dependency>
    <groupId>com.google.code.gson</groupId>
    <artifactId>gson</artifactId>
    <version>2.8.6</version>
</dependency>
```

是一样的，但是 Gradle 的会更加简洁一些，并且可以通过代码动态控制版本或者是其他的东西。

在这里需要注意一下 `compile`，Gradle 提供了许多应用依赖的方式：

* compile：在高版本 Gradle 中将被废弃，请使用 `api` 代替
* api：对当前模块和引用该模块的其他模块都可见
* implementation：仅对当前模块可见

`api` 一般用于库项目，而 `implementation` 一般用于应用项目。

## 任务系统

除了管理依赖，另一个 Gradle 的功能就是 **任务系统** 了：

通过 `task` 创建一个任务：

```groovy
// Groovy 风格
task hello {                    // 创建一个 “hello” 任务
    println 'hello'             // **配置** 任务

    doLast {                    // 在任务最后执行
        println 'last'  
    }

    doFirst {                   // 在任务一开始执行
        println 'first'
    }
}

// Java 风格
task("hello-java", new Action<Task>() {
    @Override
    void execute(Task task) {
        task.doFirst {
            System.out.println("Hello, Java!");
        }
    }
})
```

随后可以通过命令 `./gradlew hello` 来调用任务

```plain
> Configure project :
hello

> Task :hello
first
last

BUILD SUCCESSFUL in 18s
1 actionable task: 1 executed
```

会输出类似以上内容。

需要注意的是，**配置**任务在每次调用 root 项目的 build.gradle 时都会执行，即使没有调用 hello 任务：

```plain
> ./gradlew hello-java

Configuration on demand is an incubating feature.

> Configure project :
hello

> Task :hello-java
Hello, Java!

BUILD SUCCESSFUL in 3s
1 actionable task: 1 executed
```

### 有向无环图

一些任务通常需要依赖其他任务，比如吃东西前需要拿到食物和张嘴：

```groovy
task tabemono {
    doLast {
        println 'Get Tabemono'
    }
}

task openmouth {
    doLast {
        println 'Opened Mouth'
    }
}

task eat {
    dependsOn tabemono, openmouth           // 使用 dependsOn 函数进行任务依赖

    doLast {
        println 'Oishii!'
    }
}
```

```plain
./gradlew eat

> Configure project :
hello

> Task :openmouth
Opened Mouth

> Task :tabemono
Get Tabemono

> Task :eat
Oishii!

BUILD SUCCESSFUL in 3s
3 actionable tasks: 3 executed
```

## 项目的海洋

Gradle 还支持子项目：

```plain
root
  - build.gradle                // root 项目的配置文件
  - settings.gradle
  - sub
      - build.gradle            // sub 项目的配置文件
```

随后在 `settings.gradle` 中：

```groovy
include ':sub'          // 导入 sub 项目
```

`sub/build.gradle`

```groovy
task subhello {
    doLast {
        println 'I am a subproject!'
    }
}
```

```plain
> ./gradlew :sub:subhello

> Task :sub:subhello
I am a subproject!

BUILD SUCCESSFUL in 16s
1 actionable task: 1 executed
```

### 地图炮

有的时候需要对一堆子项目配置相同的内容，可以使用 `subprojects`：

`build.gradle`

```groovy
plugins {
    id 'java'
}

subprojects {
    apply plugin: 'java'            // plugins 只能在顶层使用，在其他地方需要使用 apply 函数
}
```

## 最后

参考：

* [Gradle](https://gradle.org)
