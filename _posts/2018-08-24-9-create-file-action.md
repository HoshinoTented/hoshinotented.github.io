---
layout: post
title: IntelliJ 系列插件开发 第9节 创建文件的 Action
post: YES!
---

# 时光倒流
每次都要手动加上扩展名...  
好麻烦  
有没有什么办法可以单独弄一个 `Action` 去创建我们的 `.hsn` 文件呢  

# 回到现在
当当!  
看看星野找到了什么?  
**`CreateFileAction`**  
没错没错, 就是创建文件的 `Action`  
还记得 `Action` 得在哪个包里创建吗?  
`action/HoshinoCreateFileAction.java`
```Java
public class CreateHoshinoFileAction extends CreateFileAction {
	public CreateHoshinoFileAction() {
		super("Hoshino File", "Create a Hoshino File", HoshinoIcons.hoshino);
	}

	@Nullable
	@Override
	protected String getDefaultExtension() {
		return HoshinoLanguage.EXTENSTION;
	}
}
```
那么先看看这里  
```Java
super("Hoshino File", "Create a Hoshino File", HoshinoIcons.hoshino);
```
这里调用了父类的构造器, 设置了 `name`, `description` 和 `icon`  
酱紫写的话, 在 `plugin.xml` 注册 `Action` 就可以只写 `class` 和 `id` 了  
那么接下来是...  
这个!  
```Java
@Nullable
@Override
protected String getDefaultExtension() {
	return HoshinoLanguage.EXTENSTION;
}
```
这个方法是需要返回一个 `文件扩展名`  

# 注入灵魂
`plugin.xml`  
```xml
<action id="Hoshino.CreateHoshinoFile"
		class="org.hoshino9.intellijplugin.action.CreateHoshinoFileAction">
	<add-to-group group-id="NewGroup1"/>
</action>
```

# 抵达未来
嗯哼, 那么一切都已经完成了  
启动 IDE, 试着通过 `CreateHoshinoFileAction` 创建一个 `.hsn` 文件吧  