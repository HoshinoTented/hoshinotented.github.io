---
layout: post
title: 脚把脚教你打造简单的 Gradle 插件
post: YES!
---

## 准备

开发 Gradle 插件前，当然是准备开发工具，这里是推荐使用 [IntelliJ IDEA](https://www.jetbrains.com/idea/)  

## 开始开发

### 项目格式

根据官网所说，有三种方式来开发 Gradle 插件  

* Build Script  
  直接将代码写到 Build Script 中，可以直接测试  
* buildSrc  
  将代码写到 `buildSrc` 模块中，同样可以直接测试  
* 独立项目  
  将代码写到独立项目中，需要使用 `gradle install`（需要应用 maven 插件）安装到本地的 maven 仓库  
  然后在另一个项目使用  

开发语言也是各式各样，这里使用 ~~比 Java 强不知道几千倍的超级~~ 静态语言 Kotlin 进行开发  
在这篇文章中，由于本小姐更喜欢独立项目，所以就用独立项目来进行开发啦  

### 第一个插件

首先需要添加 `Gradle` 的依赖  
file: `build.gradle.kts`  

```kotlin
dependencies {
    compile(gradleApi())
}
```

接着就能愉快地开发插件了  

一个 Gradle 插件至少需要一个 `Plugin` 接口的实例，所以接下来我们要创建一个实现类，来实现 `Plugin` 这个接口  
`Plugin` 接口中有一个名为 `apply` 的方法，这个方法会在插件被应用到项目时被调用  

首先在 `src/main/kotlin/org/hoshino9/gradle/HoshinoPlugin.kt` 中写下如下代码  
```kotlin
package org.hoshino9.gradle

import org.gradle.api.Plugin
import org.gradle.api.Project

class HoshinoPlugin : Plugin<Project> {
    override fun apply(project: Project) {
        project.run {
            task("whoCute") {       // 新建一个任务
                it.doLast {
                    println("Hoshino is so cute")       // 在任务的最后输出一条语句
                }
            }
        }
    }
}
```

这样，我们就成功地创建了一个普通的插件  

### 让 Gradle 找到插件

`Gradle` 要如何找到插件的实现类？  

#### 添加 .properties 文件  

你可以在 `src/main/resources/META-INF/gradle-plugins` 文件夹下添加插件的 `<plugin-id>.properties` 文件  
内容是 `implementation-class=你的 Gradle 插件实现类的全限定名`  
file: `src/main/resources/META-INF/gradle-plugins/org.hoshino9.gradle.plugin.properties`  

```properties
implementation-class=org.hoshino9.gradle.HoshinoPlugin
```

#### 使用 java-gradle-plugin  

首先在 `build.gradle.kts` 下添加插件:  

```kotlin
plugins {
    // ...
    `java-gradle-plugin`
    // ...
}
```

然后添加代码:  

```kotlin
gradlePlugin {
    plugins {
        create("hoshino") {     // 插件名称，目前还不知道干什么用的...
            id = "org.hoshino9.gradle.plugin"       // 插件的 id
            implementationClass = "org.hoshino9.gradle.HoshinoPlugin"                // 插件的实现类
        }
    }
}
```

在构建的时候就会自动生成 `.properties` 文件了  

### 使用插件

由于本小姐是个天才，所以使用独立项目来开发 Gradle 插件，所以不得不做一些天才才需要做的操作  
首先在开发插件所在模块的 `build.gradle.kts` 下添加如下代码  

```kotlin
plugins {
    // ...
    maven
    // ...
}
```

然后在命令行运行:  

```bash
./gradlew install
```

接着，Gradle 插件就会被安装到本地的 maven 仓库  
然后，再创建一个新的模块，并在它的 `build.gradle.kts` 下写下:  
file: `test/build.gradle.kts`  

```kotlin
buildscript {
    repositories {
        mavenLocal()        // 使用本地 maven 仓库
    }

    dependencies {
        classpath("org.hoshino9.gradle.plugins:hoshino-plugin:1.0")       // 使用插件，格式为: "插件项目的group:插件项目的name:插件项目的version"
    }
}

apply(plugin = "org.hoshino9.gradle.plugin")      // 应用插件，格式为: "插件的id"
```

然后就可以愉快地使用自己编写的插件了  
比如在命令行中:  

```kotlin
> ./gradlew :test:whoCute
...
Hoshino is so cute
...
```

### 使用 Extension 传递信息

时常需要让 build script 向插件传递信息，那要怎么做呢？  
答案是使用 Extension  
file: `src/main/kotlin/org/hoshino9/gradle/HoshinoPluginExtension.kt`

```kotlin
// 注意这里的 open
open class HoshinoPluginExtension {
    var whoCute: String = "Hoshino is cute!!!!!!!!!!!!!!!!!!!!!!"
}
```

接着，在插件中创建一个 extension  
file: `src/main/kotlin/org/hoshino9/gradle/HoshinoPlugin.kt`  

```kotlin
package org.hoshino9.gradle

import org.gradle.api.Plugin
import org.gradle.api.Project

class HoshinoPlugin : Plugin<Project> {
    override fun apply(project: Project) {
        val hoshino = project.extensions.create("hoshino", HoshinoPluginExtension::class.java)      // 创建 extension

        project.run {
            task("whoCute") {
                it.doLast {
                    hoshino.whoCute.run(::println)      // 使用 extension
                }
            }
        }
    }
}
```

接着在 `test/build.gradle.kts` 中修改 extension 的值  

```kotlin
// ...

val hoshino by extensions       // 不推荐官网介绍的做法，那样只能修改找到第一个 extension

hoshino.whoCute = "Hoshino is very very very ... cute!"
```

然后在命令行运行:  

```bash
./gradlew :test:whoCute
...
Hoshino is very very very ... cute!
...
```

这样就成功地修改了 extension 的值了  

### 第一个 Task

任务系统是 Gradle 的重要组成部分，接下来就教你如何创建一个简单的 Task  
一个简单的 Task 需要实现 Task 接口，但 Gradle 库中已经提供了 DefaultTask，所以我们继承这个 DefaultTask 就可以了  
file: `src/main/kotlin/org/hoshino9/gradle/HoshinoTask.kt`  

```kotlin
package org.hoshino9.gradle.learn

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

// 注意这里的 open
open class HoshinoTask : DefaultTask(), Runnable {      // Runnable 不是必要的
    @TaskAction
    override fun run() {        // 任务的主要 action 要用 @TaskAction 标记  
        println("Hoshino is cute!!!!!!!!!!!!!!!!!!!")       // 任务主体
    }
}
```

然后在 `test/build.gradle.kts` 中注册任务  

```kotlin
task.register<HoshinoTask>("hoshino")
```

命令行执行:  

```bash
./gradlew :test:hoshino
...
Hoshino is cute!!!!!!!!!!!!!!!!!!!
...
```

## 结尾

以上就是开发 **简单** Gradle 插件的内容了  
~~会不会有后续本小姐也不知道~~  

----------------
参考:  

* [gradle-plugin-example](https://github.com/jonathanhood/gradle-plugin-example/blob/master/tutorial/1-your-first-gradle-plugin.md)
* [Gradle - Custom Plugins](https://docs.gradle.org/current/userguide/custom_plugins.html)
