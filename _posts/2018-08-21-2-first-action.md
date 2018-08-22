---
layout: post
title: IntelliJ 系列插件开发 第2节 创建第一个 Action
post: YES!
---

# 第一个 Action
在 `src/main/java` 目录下创建一个 `actions` 目录  
这个目录就是专门放 `Action` 的地方啦  
接下来, 创建一个文件  
嗯。。叫什么呢。。。  
`HelloAction.java`
```Java
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

public class HelloAction extends AnAction {
	@Override
	public void actionPerformed(AnActionEvent e) {
		System.out.println("Hello world!");
	}
}
```
咱们先看看这儿
```Java
@Override
public void actionPerformed(AnActionEvent e) {
	System.out.println("Hello world!");
}
```
看名字就知道了嘛, 就是 `Action` 被触发时所执行的东西  
星野这里写了 打印一句 "Hello world!"  
<del>果然学什么都要先学 "Hello world!"</del>  
如果按照正常设置的话, 这个类的名字会泛黄(警告)  
![](https://raw.githubusercontent.com/HoshinoTented/hoshinotented.github.io/master/resources/first-action-class-warning.png)   
Yes Yes Yes!!!  
只写了 `Action`, 还没有告诉 `IDEA` 如何去触发这个 `Action` 呢  

`plugin.xml`
```xml
<actions>
    <action
			class="org.hoshino9.intellijplugin.actions.HelloAction"
			id="Hoshino.HelloAction"
			text="Say Hello"
			description="Print Hello world!">
		<add-to-group group-id="EditorPopupMenu1" anchor="first"/>
		<keyboard-shortcut first-keystroke="alt h" keymap="$default"/>
	</action>
</actions>
```
需要在 `plugin.xml` 的 `actions` 节点下写出这些东西  
* class `AnAction` 的全限定名
* id `AnAction` 的 `id`, 一般为 `插件id.ActionId`
* text `AnAction` 显示出来的名字
* description `AnAction` 的介绍

`action` 节点内的东西
* add-to-group 把 `AnAction` 添加到组
    * group-id 目标组id, 比如 `EditorPopupMenu1`, 就是在一个编辑器右键的时候, 第二个组内可以看到这个 `Action`
    * anchor `Action` 的位置, 这里是组的最前端
* keyboard-shortcut 快捷键
    * first-keystroke 默认的快捷键, 每个按键用空格分隔, 比如这里是 `alt h`, 对应的就是 `Alt+H` 这个快捷键啦
    * keymap 要添加到的 `keymap`, 一般还是 `$default` 吧

# 启动第一个 IntelliJ 插件
激动人心的时刻到了!!  
打开右侧的 `Gradle`  
\<项目名\> -\> Tasks -\> intellij  
找到 `runIde` 这个 `task` 并运行它  
接着, 将会出现一个沙盒版的 `IntelliJ IDEA`  
随便打开一个 `Project`  
等待准备完成之后  
按下 `Alt H`  
再回头看看运行输出  
是不是出现了 `Hello world!`  
![](https://raw.githubusercontent.com/HoshinoTented/hoshinotented.github.io/master/resources/first-action-hello-world.png)  
或者打开一个文件, 右键  
是不是看到了 `Say Hello`  
点击它也可以让这个 `Action` 被触发