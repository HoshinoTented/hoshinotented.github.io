---
layout: post
title: 软件层 =解决=> 硬件层.问题
post: YES!
---

# 先夸自己一下
我太厉害了  
我用软件层解决了硬件层的故障

# 什么问题来着..?
就是上次屏幕一不小心被我摔坏的那个东西。。  
那次摔坏导致花屏  
花屏的颜色和透明度是由 从手机顶部到底部, 其中最接近 #FFFFFF的像素点决定的  
越接近, 花屏透明度越高, 如果是#FFFFFF就不花屏  
所以我<del>百度</del>谷歌了悬浮窗  
把一个layout的搞上1dp的#FFFFFF颜色的边框  
酱紫就不会花屏惹  
哈哈哈哈哈哈

# 码代码的过程...
```Kotlin
val manager = getSystemService(Context.WINDOW_SERVICE) as WindowManager     //这个是WindowManager
val params = WindowManager.LayoutParams(
   matchParent, matchParent,        //全屏
   0, 0,        //从左上角开始
   WindowManager.LayoutParams.TYPE_SYSTEM_OVERLAY,      //在所有界面之上, 之前用了TYPE_SYSTEM_ALERT, 发现状态栏拉下来之后会被覆盖掉
   WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE or WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,      //不接受触摸和不接受焦点, 如果不设置不接受焦点会导致输入法起不来。。。
   PixelFormat.RGBA_8888)       //背景, 当然是透明啦

manager.addView(LinearLayout(this).apply {      //加东西
   backgroundResource = R.drawable.border
}, params)
```

```xml
<?xml version="1.0" encoding="utf-8"?>
<shape xmlns:android="http://schemas.android.com/apk/res/android">
    <stroke
        android:color="#FFFFFF"
        android:width="1dp"/>
        
        <!-- color 那个是白色 -->
        <!-- width 是宽度, 为了不影响使用体验(挡到东西), 所以就搞成1dp了 -->
</shape>
```

# 之后...
好吧还是会花, 不过没那么严重, 至少不用买新手机（  
间接赚钱  
我好棒