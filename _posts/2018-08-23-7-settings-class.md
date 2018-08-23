---
layout: post
title: IntelliJ 系列插件开发 第7节 创建一个 Setting 类
post: YES!
---

# Setting类
`Setting` 类并不是必要的, 只是会让插件开发过程更加的**友好**  
`Setting` 类不需要继承任何东西, 如果想继承的话也是可以的  
`Setting` 类只用来存储设置信息  

`HoshinoSetting.java`
```Java
class HoshinoSetting {
    public String name;
}
```
`name` 字段对应 `ModuleWizard` 的 `name` 输入框  

# 把 ModuleWizard 的数据传给 ModuleBuilder
其实这个很简单的啦  
**这些内容只是实例而已, 并不一定需要这样写哦**  
~~先把上次弄上的50个界面删掉再说。。~~  
修改: `HoshinoModuleBuilder.java`  
```Java
public class HoshinoModuleBuilder extends ModuleBuilder {
	public HoshinoSetting setting = new HoshinoSetting();
	
    @Override
	public void setupRootModel(ModifiableRootModel modifiableRootModel) throws ConfigurationException {
		doAddContentEntry(modifiableRootModel);
		ContentEntry entry = modifiableRootModel.getContentEntries()[0];
		try {
			VirtualFile moduleRoot = entry.getFile();
			if (moduleRoot != null) moduleRoot.findOrCreateChildData(modifiableRootModel.getModule(), setting.name);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/*...*/
	
	@Nullable
    @Override
    public ModuleWizardStep getCustomOptionsStep(WizardContext context, Disposable parentDisposable) {
    	return new HoshinoModuleWizard(this);
    }
}
```

修改: `HoshinoModuleWizard.java`  
```Java
public class HoshinoModuleWizard extends HoshinoModuleWizardUI {
	private HoshinoModuleBuilder builder;

	public HoshinoModuleWizard(HoshinoModuleBuilder builder) {
		this.builder = builder;
	}

	/*...*/

	@Override
	public void updateDataModel() {
		builder.setting.name = this.name.getText();
	}
}
```
主要看看 `HoshinoModuleBuilder` 的 `setupRootModel` 方法  
这里获取到了 `modifiableRootModel` 的第一个 `ContentEntry`  
也就是被创建的 `Module` 本身  
之后通过 `getFile` 获取到了这个 `Module` 的 `VirtualFile`  
使用 `findOrCreateChileData` 创建了一个文件  
关于 `VirtualFile` 这一块可能以后会讲到emmm  
~~也可能不会, 因为星野也只是接触也已丫~~  

最后, 打开 IDE  
创建一个 `Hoshino Module`  
在输入框里输入 `hoshino`  
是不是在 `Module` 下面出现了一个名为 `hoshino` 的文件呢