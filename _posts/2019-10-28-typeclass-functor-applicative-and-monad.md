---
layout: post
title: 函子，可应用函子和单子
post: YES!
---

类型类是 Haskell 中重要的组成部分，它类似于 Java 中的**接口**，接下来就让我们一起来看看奇妙的类型类吧。

## 什么是类型类

Haskell 中可以把具有共同属性（或叫做特征）的类型归为一类，这一类就被称为类型类。
比如常见的 `Eq` 类型类，代表了一类可以进行 **比较相等和不相等** 的类型，或是 `Ord` 类型类，代表了一类可以进行 **比较大小** 的类型。

## 函子（Functor）

在这之前先向一些新读者介绍一下 `map` 函数，`map` 函数是将一个列表的每一个值都应用上一个函数，然后将应用函数的结果再组成一个列表。很明显，`map` 函数只会改变列表的类型，而不会改变列表的长度。

```haskell
map :: (a -> b) -> [a] -> [b]
```

当然不止列表，树、集合，等等的许多类型都可以有自己的 map 函数，在没有类型类的情况下，就需要对每个类型都定义一个属于它们自己的 `map` 函数。

```haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = ...

mapSet :: (a -> b) -> Set a -> Set b
mapSet = ...
```

而使用类型类，就是将这些散乱的 `map` 函数统一（抽象）起来，于是便有了 `Functor` 类型类。

`Functor` 类型类只定义了一个函数：`fmap`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

抽象出来的 `fmap` 不仅能用于列表，还能用于几乎所有的容器类型，比如最简单的 `Identity` 类型。

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

不太简单的 `Maybe` 类型。

```haskell
data Maybe a = Nothing | Just a
```

非常不简单的 `State` 类型（注意：mtl 库中并不是这么定义的，只是为了方便才这么写）

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

Functor 可以看做是你有一个你无法打开的魔法盒子（f a），但是你想对盒子里的数值（a）应用一个函数（a -> b），这个时候就可以求助 `fmap`，让它帮你把函数应用到数值上，然后把结果再放进盒子里，还给你。

```haskell
> (+1) `fmap` (Just 1)
Just 2
> (+1) `fmap` Nothing
> Nothing
```

为了方便，Haskell 标准库中定义了一个和 `fmap` 等价的 `(<$>)` 运算符。

```haskell
> (+1) <$> (Just 1)
Just 2
```

让一个 Maybe Int 中的值加上 1 是简单的，但如果是想让两个 Maybe Int 相加呢？Functor 显然做不到这一点。

## 可应用函子（Applicative）

Applicative 类型类中主要有两个函数，用来把一个普通的值装进魔法盒子里的 `pure`，还有将装在魔法盒子里的函数应用到另一个装在魔法盒子里的值的 `(<*>)`。

```haskell
class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    -- ...
```

首先是较为简单的 `pure`，可以把一个普通的值装进 Applicative 中：

```haskell
> pure 1 :: Maybe Int
Just 1
```

接着是和 `fmap` 类似的 `(<*>)`，可以把一个装在 Applicative 中的函数（f (a -> b)）应用在另一个 装在 Applicative 中的值（f a）上。
比如可以通过这种方式实现两个 Maybe Int 的相加：

```haskell
> (+) <$> Just 1 <*> Just 2
Just 3
> pure (+) <*> Just 1 <*> Just 2
Just 3
```

## 单子（Monad）

