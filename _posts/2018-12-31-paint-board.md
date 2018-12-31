---
layout: post
title: 临终之日, 新年之时
post: YES!
---

# 最后一天
今天就是这一年的最后一天了  
与此同时, [你谷](https://www.luogu.org) 也迎来了冬日绘板活动  
也因此, 各类脚本层出不穷  
不过说到底, 绘板的竞争  
还是脚本的竞争  
是 `Cookie` 数量的竞争  

# 那么大小姐您做了什么呢
本小姐当然是也写了几个[脚本](https://github.com/HoshinoTented/LuoGuAPI)  
本小姐在这里面当然也学到很多东西了  
比如用来计时的 `协程`  

## 为什么要选择协程
如果是 `draw` 之后立刻 `Thread.sleep`  
会导致有大量时间浪费在这上面  
如果使用协程  
那么可以把计时任务交给一个 `Deferred`  
等到需要 `draw` 的时候再 `await`  
在我们 `draw` 完, 到 `await` 的过程中  
会有很多堵塞线的的操作  
比如跳过了部分色块(主要)  
网络访问等等  

## 那么大小姐遇到了什么坑呢
当然是这种坑:  

```kotlin
/* import ...*/

var timer = GlobalScope.async { delay(3000) }

runBlocking {
    println("begin of block")
    timer.await()
    println("reset timer")
    timer = async { delay(3000) }
    println("end of block")
}

println("finished")
```

本来的预想应该是:  
```
begin of block
[delay 3s]
reset timer
end of block
finished
```

但事与愿违:  
```
begin of block
[delay 3s]
reset timer
end of block
[delay 3s]
finished
```

之后, 在 `kotlin` 官网的协程教程上看到了这样一句话:  
> 调⽤了 runBlocking 的主线程会⼀直阻塞 直到 runBlocking 内部的协程执⾏完毕。  

本小姐的聪明的小脑袋就想, 难道 `runBlocking` 内的 `async` 也算是 `runBlocking` 的内部协程吗  
于是...  
```kotlin
/* import ...*/

var timer = GlobalScope.async { delay(3000) }

runBlocking {
    println("begin of block")
    timer.await()
    println("reset timer")
    timer = GlobalScope.async { delay(3000) }
    println("end of block")
}

println("finished")
```

运行结果就符合本小姐的预期了  

# 没了
[真的没了](/)