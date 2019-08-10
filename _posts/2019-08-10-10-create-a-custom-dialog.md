---
layout: post
title: IntelliJ 系列插件开发 第10节 打造爆款对话框
post: YES!
---

咳咳。。本小姐才没有咕咕咕。。  
虽然已经过去一年了。。  
那进入正题吧，由于最近因为 ~~想骗 JetBrains 的授权和少量的~~ 兴趣，正在开发 [luogu-intelli](https://github.com/HoshinoTented/luogu-intellij)  
所以也遇到了很多问题  
比如这次要说的: **如何创建一个对话框**  

# UI 设计
创建对话框之前需要先打造一个UI  
这里请自行百度 `IntelliJ GUI Form`  
在经过一系列的奇奇怪怪的操作之后  
会获得两个文件: `xxx.java` 和 `xxx.form`  
生成的 `Java` 文件本小姐推荐再加工一下:  

* 添加 `abstract`
* 把所有 `private` 修改成 `protected`  
* 让生成的类继承 `DialogWrapper`(记得调用父类构造器)  

# DialogWrapper
然后创建一个用来实现逻辑操作的类  

```kotlin
class MyDialogImpl : MyDialog() {
    init {
        title = "Hoshino is so cute"  // 设置标题
        init()  // 初始化窗口，必须要调用
    }

    // 实现 DialogWrapper 的抽象方法，这里返回 mainPanel，也就是 UI 中最顶层的 JPanel 对象
    override fun createCenterPanel(): JComponent? {
        return mainPanel
    }
}
```

然后在需要显示对话框的地方:  

```kotlin
MyDialogImpl().show()
```

就可以显示对话框啦  

# 结尾

更多的内容还请浏览 `DialogWrapper` 源代码呢
