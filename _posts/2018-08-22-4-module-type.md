---
layout: post
title: IntelliJ 系列插件开发 第4节 第一个 Module Type
post: YES!
---

参考: [项目向导](http://www.jetbrains.org/intellij/sdk/docs/reference_guide/project_wizard.html#module-type),
[模块](http://www.jetbrains.org/intellij/sdk/docs/reference_guide/project_model/sdk.html)

# 什么是 Module
~~TODO 重制必要~~
```
A module is a discrete unit of functionality that can be run, tested, and debugged independently. 
Modules includes such things as source code, build scripts, unit tests, deployment descriptors, etc.
```
~~谷歌~~翻译过来就是...
```
模块是一个独立的功能单元，可以独立运行，测试和调试。模块包括源代码，构建脚本，单元测试，部署描述符等。
```
~~星野真的英文不能丫QAQ~~

# 属于自己的 Module Type
相信大家看完上面的 Module 介绍之后, 对 Module 有一定的了解了 \( 并没有了解  
那么, 赶紧开始创建属于自己的 `ModuleType` 吧  
首先在 `src/main/java` 下创建一个 `module` 目录  
当然就是用于存放关于 `Module` 的东西啦  
`HoshinoModuleType.java`  
```Java
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleTypeManager;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class HoshinoModuleType extends ModuleType {
	public static final String MODULE_ID = "HOSHINO_MODULE_TYPE";
	public static final ModuleType INSTANCE = ModuleTypeManager.getInstance().findByID(MODULE_ID);

	public HoshinoModuleType() {
		super(MODULE_ID);
	}

	@NotNull
	@Override
	public ModuleBuilder createModuleBuilder() {
		return null;
	}

	@NotNull
	@Override
	public String getName() {
		return "Hoshino";
	}

	@NotNull
	@Override
	public String getDescription() {
		return "Hoshino Project";
	}

	@Override
	public Icon getNodeIcon(boolean isOpened) {
		return null;
	}
}
```
一个 `ModuleType` 当然就是需要继承 `ModuleType` 类  
`getName`, `getDescription` 和 `getNodeIcon`, 顾名思义, 就是 `Module` 的名字, 介绍和图标  
接下来看看两个被 `public final static` 修饰的属性  
`MODULE_ID` 就是我们这个 `HoshinoModuleType` 的 id, 很明显是不可以跟其他 `Module` 的 id 重复  
`INSTANCE` 就是 `HoshinoModuleType` 的实例。。  

# ModuleBuilder
话说。。上面有一个方法我们好像还没实现  
而且 `ModuleType` 也还有一个泛型参数 `<T extends ModuleBuilder>`  
那么这个 `ModuleBuilder` 到底是何方神圣呢??  

`ModuleBuilder`, 便是用于建立一个 `Module`  
`HoshinoModuleBuilder.java`  
```Java
import com.intellij.ide.util.projectWizard.ModuleBuilder;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.roots.ModifiableRootModel;

public class HoshinoModuleBuilder extends ModuleBuilder {
	@Override
	public void setupRootModel(ModifiableRootModel modifiableRootModel) throws ConfigurationException {
		doAddContentEntry(modifiableRootModel);
	}

	@Override
	public ModuleType getModuleType() {
		return HoshinoModuleType.INSTANCE;
	}
}
```

`setupRootModel` 用于建立一个 `Module` 环境, **大概**是在新建一个 `Module` 的时候触发  
`getModuleType` 当然是用于获取 `HoshinoModuleBuilder` 相对应的 `ModuleType` 啦
接下来修改一下 `HoshinoModuleType` 的某些部分  
```Java
@NotNull
@Override
public ModuleBuilder createModuleBuilder() {
	return new HoshinoModuleBuilder();
}
```

# 新建一个 Hoshino Module
最后, 运行IDE  
通过 Create New Project -\> Hoshino 创建一个 Hoshino Module  
但是。。总感觉少了什么呢。。。  
`Module` 的图标。。？