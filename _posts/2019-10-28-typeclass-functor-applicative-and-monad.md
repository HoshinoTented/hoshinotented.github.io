---
layout: post
title: 是类型类！还有函子，可应用函子和单子！
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

既然能让一个 `Just 1` “加上” 1，那能不能让两个 Maybe Int 值相加呢？

```haskell
> :t (+) <$> Just 1
(+) <$> Just 1 :: Num a => Maybe (a -> a)
```

会发现函数跑到魔法盒子里面去了，但我们目前似乎无法通过目前的任何方法把它取出来，或者应用到其他的值上。解决这个问题的，就是接下来要介绍的 Applicative。

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

假设你打算用 Haskell 写一个抽象语法树：

```haskell
data Expr = ILit Int
    | Add Expr Expr
    -- 省略无用的语法
    | Div Expr Expr
```

然后需要对它们进行求值：

```haskell
eval :: Expr -> Int
eval (ILit i) = i
eval (Add a b) = eval a + eval b
eval (Div a b) = eval a `div` eval b
```

但这明显有个问题，一旦作为除数的表达式结果为 0，那么这个表达式就会报错。
能不能做到委婉地返回一个错误值，而不是强硬地报错呢？
我们可以使用用来处理错误的类型：Maybe

```haskell
safeEval :: Expr -> Maybe Int
safeEval (ILit i) = Just i
safeEval (Add a b) = case safeEval a of
                       Nothing -> Nothing
                       Just a' -> case safeEval b of
                         Nothing -> Nothing
                         Just b' -> Just (a' + b')
safeEval (Div a b) = case safeEval a of
                       Nothing -> Nothing
                       Just a' -> case safeEval b of
                         Nothing -> Nothing
                         Just b' -> if b' == 0 then Nothing else Just (a' `div` b')
```

但会发现，有许多重复代码：

```haskell
case something of
  Nothing -> Nothing
  Just m -> something'
```

于是，可以把这些重复代码提取出来，做成一个新的函数：

```haskell
ifJust :: Maybe a -> (a -> Maybe a) -> Maybe a
ifJust Nothing _ = Nothing
ifJust (Just a) f = f a
```

然后，就可以重新编写我们的 safeEval 函数：

```haskell
-- ...
safeEval (Add a b) = ifJust (safeEval a) $ \a' -> 
  ifJust (safeEval b) $ b' ->
    Just $ a' + b'
safeEval (Div a b) = ifJust (safeEval a) $ \a' -> 
  ifJust (safeEval b) $ b' ->
    if b' == 0
      then Nothing
      else Just $ a' + b'
```

而这个 `ifJust` 函数，就类似于单子的 `(>>=)`。

```haskell
class Applicative m => Monad (m :: * -> *) where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

单子和可应用函子有一些相同之处，比如都有相同功能的 `return` 和 `pure`。
而 `(>>=)` 就像上面的 `ifJust` 函数，可以进行连续的运算并自动处理一些（上面则是 返回 Nothing 值来代表计算错误）。

```haskell
safeDiv (Div a b) = safeEval a >>= \a' ->
  safeEval b >>= \b' ->
    if b' == 0
      then Nothing
      else Just $ a' `div` b'
```

文章最开始介绍到的 `Identity` 和 `State` 也都是 Monad。

`Identity` 是最简单的 Monad，仅包含了一个值，而不做任何运算：

```haskell
> Identity 1 >>= \i -> Identity (i + 2)
Identity 3
> (+2) 1
3
```

`State` 则较为复杂，以后的文章会做讲解。

Haskell 还针对 `(>>=)` 设计了一个语法糖，能够看出 `(>>=)` 对纯函数式编程的重要性。

```haskell
foo :: IO ()
foo = getLine >>= \s -> putStrLn s

-- 等价于

foo' :: IO ()
foo' = do
  s <- getLine
  putStrLn s
```

Haskell 标准库还提供了两个函数：`liftM` 和 `ap`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f a = do
  a' <- a

  return (f a')

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ap :: Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a

  return (f' a')
```

很明显，Functor 的 fmap 和 Applicative 的 (<*>) 都可以用这两个函数来实现。

### Monad 的一些特点

Monad 的 `(>>=)` 是用于进行连续的运算，并自动处理一些重复的事情。
比如 Maybe Monad 就是自动处理错误，一旦一个环节发生错误，接下来的运算都会是错误的。
又或者 State Monad，进行连续运算的同时还维护了一个状态，十分适合在纯函数式编程中模拟变量。

不知道读者有没有发现一个现象，无论是 Functor，Applicative 还是 Monad，都没有能够把数值从这个魔法盒子里拿出来的操作，这就意味着一个值一旦进入了魔法盒子，就再也无法通过 Functor，Applicative 和 Monad 提供的操作拿出来了，这也是 IO Monad（用于处理副作用的 Monad）的一个重要性质。

## 结尾

Monad 作为纯函数式编程中十分重要的一部分，学习 Monad 是必不可少的。
有趣的纯函数和类型世界里总是充满了困难和惊喜，希望这篇文章能成为推动你学习纯函数式编程的动力。
