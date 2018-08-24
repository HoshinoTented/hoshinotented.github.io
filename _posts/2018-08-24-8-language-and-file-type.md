---
layout: post
title: IntelliJ 系列插件开发 第8节 属于自己的语言
post: YES!
---

# 开头
众所周知, `IntelliJ IDEA` 是一款**宇宙级IDE**  
那, 开发的插件大部分都跟语言有关  
这一次...  
星野就要开发一个属于自己的语言!  
和属于这个语言的插件!!  

# 正文
## Language
既然要弄一个语言的插件, 用头发想想都知道是需要一个叫做 `Language` 的类  
创建一个名为 `language` 的包  
`HoshinoLanguage.java`
```Java
public final class HoshinoLanguage extends Language {
	public static final HoshinoLanguage INSTANCE = new HoshinoLanguage();
	public static final String NAME = "Hoshino";
	public static final String EXTENSTION = "hsn";

	private HoshinoLanguage() {
		super(NAME);
	}
}
```
看起来呢, 这个类是个单例  
`NAME` 就是这个语言的名称啦  
`EXTENSTION` 就是语言的扩展名  

## LanguageFileType
既然有语言, 那么就得有对应的文件类型  
就是 `LanguageFileType` 这个东西啦!  
同样是在 `language` 包下  
`HoshinoFileType.java`
```Java
public final class HoshinoFileType extends LanguageFileType {
	public static final HoshinoFileType INSTANCE = new HoshinoFileType();

	private HoshinoFileType() {
		super(HoshinoLanguage.INSTANCE);
	}

	@NotNull
	@Override
	public String getName() {
		return HoshinoLanguage.NAME;
	}

	@NotNull
	@Override
	public String getDescription() {
		return "Hoshino File";
	}

	@NotNull
	@Override
	public String getDefaultExtension() {
		return HoshinoLanguage.EXTENSTION;
	}

	@Nullable
	@Override
	public Icon getIcon() {
		return HoshinoIcons.hoshino;
	}
}
```
这个也是个单例呢emmmm  
`getName` 获取语言文件类型的名字  
`getDefaultExtension` 获取语言文件的扩展名  
`getIcon` 获取语言文件的图标  
\(~~好困。。。~~  

## FileTypeFactory
刚出炉的 `LanguageFileType` 还不能直接食用哦  
~~要弄上玉米油, 撒点孜然, 隔壁家小孩都馋哭了~~  
需要用一个叫做 `FileTypeFactory` 的工厂类把 `FileType` 进行加工  
`HoshinoFileTypeFactory.java`
```Java
public class HoshinoFileTypeFactory extends FileTypeFactory {
	@Override
	public void createFileTypes(@NotNull FileTypeConsumer consumer) {
		consumer.consume(HoshinoFileType.INSTANCE, HoshinoLanguage.EXTENSTION);
	}
}
```

## 注册 FileTypeFactory
最后在 `plugin.xml` 里注册这个 `FileTypeFactory`  
```xml
<fileTypeFactory implementation="org.hoshino9.intellijplugin.language.HoshinoFileTypeFactory"/>
```

# 结尾
最后, 打开 IDE  
New -\> File  
输入 `hoshino.hsn`  
看看是不是有个星星的图标呢  
但是这样好麻烦。。。  
能不能像 `Java` 那样, 有一个单独的 `Action` 去创建文件呢?