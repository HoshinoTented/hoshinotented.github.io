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
![](/_includes/first-action-class-warning.png)