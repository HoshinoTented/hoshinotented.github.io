---
layout: post
title: IntelliJ 系列插件开发 第6节 给 Module 添加向导
post: YES!
---

# 眼观未来
话说。。  
其他很厉害的那种插件  
好像 `Module` 都有一个创建向导。。  
那。。  
星野的目标可是星辰大海  
~~向导不创何以闯天下~~  

# 创建向导界面
既然要完成如此**伟大的成就**  
那么就需要**宇宙级的工具**!!  
**IntelliJ IDEA**!!!!!!!!!!!!!!!!  
的 `GUI Form` 可视化功能  
`GUI Form` 是 IDEA 的一个可视化创建 Swing 界面的功能  
可以通过 New -\> GUI Form 创建  
这部分的教程。。  
星野其实也不会\(x  
[百度](https://baidu.com)/[谷歌](https://google.com) 一下应该就会有的吧  

嗯。。。星野创建了一个这样的[界面](https://github.com/HoshinoTented/UnknownPlugin/blob/master/src/main/java/org/hoshino9/intellijplugin/module/ui/HoshinoModuleWizardUI.form)  
长这样的啦  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/module-wizard-ui.png?raw=true)  
创建界面的同时, 如果没有自己设置对应的类名的话, 是会创建一个同名的类的  
看起来像这样  
```Java
public class HoshinoModuleWizardUI {
	private JPanel mainPanel;
	private JTextField name;
}
```
我们把这个类, 改为 `abstract` 抽象类, 并把 `private` 修饰符改为 `protected`  
还要继承 `ModuleWizard` 类  
这些都是为了下面的东西做准备的  

# 创建向导实现类
有了界面  
接下来就是逻辑的实现啦  
在 `module` 包下创建一个 `HoshinoModuleWizard` 类, 继承 `HoshinoModuleUI`  
```Java
import org.hoshino9.intellijplugin.module.ui.HoshinoModuleWizardUI;

import javax.swing.*;

public class HoshinoModuleWizard extends HoshinoModuleWizardUI {
	@Override
	public JComponent getComponent() {
		return this.mainPanel;
	}

	@Override
	public void updateDataModel() {
		
	}
}
```
首先看看 `getComponet`, 需要返回一个 `JComponet`  
而这个 `JComponet` 对象自然就是向导的界面了  
接下来是 `updateDataModel`, 是当这个向导结束之后所需要做的工作  
比如录入设置等等  

# 添加向导
接下来  
就是最后的一步了!!  
完成这一步, 星野就又离星辰大海近了一步  
把 向导实现类 添加到 `ModuleBuilder`  
需要往 `HoshinoModuleBuilder` 添加内容  
```Java
@Nullable
@Override
public ModuleWizardStep getCustomOptionsStep(WizardContext context, Disposable parentDisposable) {
	return new HoshinoModuleWizard();
}
```
`getCustomOptionsStep` 是自定义了 Module 创建的 **第一个界面**  
启动 IDE 之后, 在创建 Module 的界面, 会看到像这样的东西  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/module-wizard-home.png?raw=true)  
但是如果不想要在第一个界面要怎么办呢。。  
翻阅源码, 会发现一个这样的方法  
```Java
public abstract ModuleWizardStep[] createWizardSteps(@NotNull WizardContext wizardContext, @NotNull ModulesProvider modulesProvider);
```
这个就是把向导界面放在 **第一个界面** 之后的方法了  
比如星野给它。。  
弄上50个界面!  
`HoshinoModuleBuilder.java`
```Java
@Override
public ModuleWizardStep[] createWizardSteps(@NotNull WizardContext wizardContext, @NotNull ModulesProvider modulesProvider) {
	int size = 50;
	HoshinoModuleWizard[] wizards = new HoshinoModuleWizard[size];
	for (int i = 0; i < size; ++ i) {
		wizards[i] = new HoshinoModuleWizard();
	}

	return wizards;
}
```
点不完了...