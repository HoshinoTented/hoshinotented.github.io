---
layout: post
title: IntelliJ 系列插件开发 第1节 创建一个插件工程
post: YES!
---

# 创建一个插件工程
应该不用星野教吧。。  
百度一堆来着。。  
如果执意要星野教的话也不是不可以。。  
首先。。打开 IntelliJ IDEA  
Create New Project -> Gradle  
勾选 IntelliJ Platform Plugin  
星野选择 Gradle 是因为。。  
<del>只会用 Gradle</del>  
使用 Gradle 会更方便一点
嗯没错  
Next 之后就是一些平时都见到过的东西啦...

# 创建之后要做的事情
**等**  
Gradle 会下载一堆奇奇怪怪乱七八糟五颜六色的东西  
在等待的闲暇我们先看一些待会需要的东西吧  
`build.gradle`
很明显这是必须要看的  
```Groovy
plugins {
    id 'java'
    id 'org.jetbrains.intellij' version '0.3.6'     //开发插件的插件
}

group 'org.hoshino9'        //星野的域名
version '1.0-SNAPSHOT'      

sourceCompatibility = 1.8

repositories {
    mavenCentral()
}

dependencies {
    testCompile group: 'junit', name: 'junit', version: '4.12'
}

//插件的配置
intellij {
    //使用的 IntelliJ Platform SDK 版本
    version '2018.2.1'
    //因为我国的某些网络特性, 可能导致这个下载的非常慢
    //所以星野推荐使用 localPath 'InteliJ IDEA的目录'
    //例如这样
    //localPath '/home/hoshino/Document/IntelliJ'
    //请根据自己的实际情况改变字符串的内容哦
}

//plugin.xml的一些修改
//比如 `changeNotes` 就是 `change-notes` 节点的东西
//`pluginDescription` 就是 `description` 节点的东西
patchPluginXml {
    changeNotes """
      Add change notes here.<br>
      <em>most HTML tags may be used</em>"""
}
```

`src/main/resources/META-INF/plugin.xml`  
插件的重要部分!!  
为了缩短篇幅, 星野把不必要的内容和注释都删除了
```xml
<idea-plugin>
  <id>插件的id</id>
  <name>插件的名字</name>
  <version>插件的版本, 例如 1.0</version>
  <!--一些开发者信息了-->
  <vendor email="support@yourcompany.com" url="http://www.yourcompany.com">YourCompany</vendor>

  <description><![CDATA[
      一些介绍, 可以使用html
    ]]></description>

  <change-notes><![CDATA[
      更新日志, 可以使用html
    ]]>
  </change-notes>

  <idea-version since-build="173.0"/>

  <extensions defaultExtensionNs="com.intellij">
    <!-- Add your extensions here -->
  </extensions>

  <actions>
    <!-- Add your actions here -->
  </actions>

</idea-plugin>
```

# 回头看看 IntelliJ IDEA
终于下完了  
不过好像也差不多该结束这一节了  
怎么办呢。。。\( 偷溜
