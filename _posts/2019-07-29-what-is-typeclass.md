---
layout: post
title: 浅谈 Haskell 类型类
post: YES!
---
    
本文将介绍如何定义类型类，并浅谈 Haskell 中内置类型类的使用  

# 什么是类型类
实现了某个类型类的类型就具有了该类型类的一些行为  
比如实现了数字类型类(`Num`)就可以进行加减乘法  

## 定义类型类
类型类使用 `class` 关键字定义
```haskell
data OIer = Hoshino | Ice1000
data OJ = IOI | NOI | NOIP

class OI a where
    ak :: a -> OJ -> Bool
    score :: a -> OJ -> Int
    baoZero :: a -> OJ -> Bool
```
我们定义了一个 `OI` 类型类  
它拥有三个函数，一个是 `ak`，一个是 `baoZero`，还有一个是 `score`

## 为类型实现类型类
使用 `instance` 关键字为类型实现类型类  
```haskell
instance OI OIer where
    score Hoshino _ = 0
    score Ice1000 NOIP = 400
    score Ice1000 _ = 100

    ak oier oj = score oier oj == 400
    baoZero oier oj = score oier oj == 0
```
然后试着调用 `ak`，`score` 和 `baoZero` 函数  
```haskell
> Hoshino `ak` IOI
False
> Ice1000 `ak` NOI
False
> score Ice1000 NOI
100
> baoZero Ice1000 NOIP
False
```

## 默认定义
我们发现，`ak` 和 `baoZero` 的实现对于是依赖于 `score` 的  
于是，我们可以提供默认定义  
```haskell
class OI a where
    ak :: a -> OJ -> Bool
    score :: a -> OJ -> Int
    baoZero :: a -> OJ -> Bool
    {-# MINIMAL score #-}

    ak oier oj = score oier oj == 400
    baoZero oier oj = score oier oj == 0

instance OI OIer where
    score Hoshino _ = 0
    score Ice1000 NOIP = 400
    score Ice1000 _ = 100
```
添加了 `{-# MINIMAL score #-}`，代表至少要实现 `score` 函数  
然后提供了 `ak` 和 `score` 的默认定义  

类型类有些类似 Java 的接口，但类型类不需要在声明时实现  
这意味着随时都能对一个类型进行扩展  
