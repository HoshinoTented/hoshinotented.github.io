---
layout: post
title: 你好，Gradle！
post: YES!
---

Gradle 是一种构建工具，可以通过编写脚本来做到依赖管理等等……

目前 Gradle 的脚本所支持的语言有 Groovy 和 Kotlin。

> Groovy 支持大部分 Java 语法，所以完全可以把 Groovy 当做 Java 用

## What Gradle can do?

Gradle 的用途很广泛，许多项目都是通过 Gradle 构建的，比如 Android Studio 就使用 Gradle 作为默认的构建工具，Minecraft Forge 也使用 Gradle 来构建项目。

## Gradle Project Structure

```plain
root
  - gradle/             // 项目级别的 Gradle Wrapper 配置
  - .gradle/
  - src/                // 存放源代码（一般称作源集：SourceSet）的文件夹
  - gradlew             // 和下面的批处理文件一样，是启动 Gradle 的脚本
  - gradlew.bat
  - build.gradle        // Gradle **模块**的脚本文件
  - settings.gradle     // Gradle **项目**的配置文件
                        // 一个 Gradle 项目只能有一个 settings.gradle，但可以有多个模块
```

## Gradle with Java

配置 Java 项目是 Gradle 最常见的用途。

你需要在 `build.gradle` 文件头加上以下内容：

```groovy
plugins {       // 配置插件的代码
    id 'java'   // 添加 Java 插件
}
```

在 `./src/main/java` 中添加 Java 源代码：

`src/main/java/Main.java`

```java
public class Main {
	public static void main(String[] args) {
		System.out.println("Hello, world!");
	}
}
```

但是 Gradle 不知道要如何运行这些代码，于是需要用到 `application` 插件：

`build.gradle`

```groovy
plugins {
    /* …… */
    id 'application'
}

application {       // 配置 application 插件
    mainClassName = "Main"      // 配置主类
}
```

随后在命令行中：

```bash
./gradlew run

> Task :run
Hello, world!

BUILD SUCCESSFUL in 4s
```

很正常地运行了代码。

## Dependencies Manage by Gradle

Gradle 的另一个重要功能是依赖管理，可以通过一行代码向项目中添加依赖：

`build.gradle`

```groovy
repositories {      // 配置依赖源
    mavenCentral()  // maven 中心仓库
}

dependencies {      // 配置依赖
    implementation group: 'com.google.code.gson', name: 'gson', version: '2.8.6'        // 添加 gson 依赖，版本为 2.8.6
}
```

接着就可以在源代码中使用 Gson 库各种功能了：

```java
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

public class Main {
	public static void main(String[] args) {
		String json = "{ \"hoshino\": \"cute\" }";
		JsonElement elem = JsonParser.parseString(json);

		System.out.println(elem.getAsJsonObject().get("hoshino").getAsString());
	}
}
```

## Task and task

Gradle 的一个重要元素就是 **任务(task)**，可以通过以下方式创建一个任务：

`build.gradle`

```groovy
// Groovy Style
task hello {
    doFirst {
        println "Hello, Groovy!"
    }
}

// Java Style
task("hello-java", new Action<Task>() {
    @Override
    void execute(Task task) {
        task.doFirst {
            System.out.println("Hello, Java!");
        }
    }
})
```

随后在同目录中，使用命令：

```bash
./gradlew hello
```

和

```bash
./gradlew hello-java
```

任务之间还可以有依赖关系：

```groovy
task("hello-java", /* …… */).dependsOn(hello)
```

这样子，调用 `hello-java` 之前就会先调用 `hello`：

```bash
./gradlew hello-java

> Task :hello
Hello, Groovy!

> Task :hello-java
Hello, Java!

BUILD SUCCESSFUL in 3s
```

## Gradle with C++

在开头就说过了，Gradle 的用途非常广泛，除了构建 Java 项目，还可以构建 C++ 项目：

和 Java 项目一样，需要添加 C++ 的插件：

`build.gradle`

```groovy
plugins {
    id 'cpp-appliction'
}

application {
    targetMachines.add(machines.windows.x86_64)
}
```

或者直接使用 `gradle init` 来创建 C++ 项目。

随后使用 `./gradlew build` 命令进行构建。构建完毕后，会在 `./build/exe/` 目录下生成可执行文件。

~~不过不怎么好用，还是老老实实用 CMake 吧~~

## Multi Module

Gradle 还支持模块化，为不同的功能创建不同的模块，最后统一导入到主模块中。

`settings.gradle`

```groovy
include ':lib'
```

接着创建一个 `lib` 文件夹，并创建对应的 `build.gradle` 文件：

```plain
root
  - lib/
      - src/
      - build.gradle
  - build.gradle
```

同样向 `lib` 模块的 `build.gradle` 中添加插件：

`lib/build.gradle`

```groovy
plugins {
    id 'java'
}
```

`lib/src/main/java/Lib.java`

```java
public class Lib {
    public static void foo() {
        System.out.println("Hello, world!");
    }
}
```

在主模块中添加对子模块的依赖：

`build.gradle`

```groovy
dependencies {
    implementation project(':lib')
}
```

`src/main/java/Main.java`

```java
public class Main {
	public static void main(String[] args) {
		Lib.foo();      // 引用 lib 模块的内容
	}
}
```

使用 `./gradlew run` 命令之后，输出了预期的内容。

不过，一旦子模块多了起来，就要向每个子模块中添加 Java 插件，很麻烦。
所以，可以使用 `allprojects` 和 `subprojects` 函数，来进行统一配置：

`build.gradle`

```groovy
subprojects {
    apply plugin: 'java'        // 注意，在 allprojects 和 subprojects 模块中，不能使用 plugins 块。
}
```

然后删去 `lib/build.gradle`，再次运行，输出了预期的内容。

## 最后

Gradle 是一种很强大的构建工具，如果你使用的是运行在 JVM 上的语言，并且对工程很有兴趣的话，Gradle 是一个不错的技能点。

## 小建议

推荐使用 IntelliJ IDEA 作为 IDE。
