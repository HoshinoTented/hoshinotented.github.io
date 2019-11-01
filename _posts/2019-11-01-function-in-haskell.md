---
layout: post
title: Haskell 中的函数
post: YES!
---

这一次来讨论一下 Haskell 中的函数。

## 函数是什么

函数主要由两部分组成：函数的类型签名和函数实现。

### 函数的类型签名

类型签名是非必要的，Haskell 可以从函数的实现中推导出函数的类型签名。

```haskell
foo :: Num a => a -> a -> a
```

这是一个名为 `foo` 的函数，它的函数签名是 `Num a => a -> a -> a`，可见函数签名主要由 类型限定（Constraint） 和 函数类型 组成。
`Num a` 即为类型限定，它限定了 `a` 类型必须是 `Num` 类型类的实例，而 `a -> a -> a` 则是函数的类型，意味传入一个 `a` 类型的值，返回一个 `a -> a` 类型的函数，而 `a -> a` 类型的函数类型又意味着传入一个 `a` 类型的值，返回一个 `a` 类型的值。这种叫做**柯里化函数**。

```haskell
> :t foo 1
foo 1 :: Num a => a -> a

> :t foo 1 2
foo 1 2 :: Num a => a
```

当函数有多个参数时，参数可以一个一个依次传入，如果参数不足，则会返回另一个函数作为结果。

### 函数的实现

```haskell
foo a b = a + b
```

这是函数的实现，但我更愿意把这看做是一种语法糖，原本的写法应该是这样的：

```haskell
foo = \a -> \b -> a + b
```

这样更能体现出柯里化。

### 模式匹配

函数的实现还提供了许多便利的功能，**模式匹配（Pattern Matching）**就是其中之一，可以通过构造器来解构出值中包含的其他值：

```haskell
fst :: (a, b) -> a
fst (a, b) = a

-- 对于不需要的参数，可以用下划线代替

head :: [a] -> a
head (x:_) = x
```

### 守卫模式

还有守卫模式，它类似于 **if-elseif-else**：

```haskell
level :: Int -> Char
level i
    | i < 0 || i > 100 = error "out of bounds"
    | i < 60 = 'E'
    | i < 70 = 'D'
    | i < 80 = 'C'
    | i < 90 = 'B'
    | i < 100 = 'A'
    | otherwise = 'S'
```

用 `|` 代表一个分支，随后衔接条件，在等号后编写相应的实现。
要注意的是 `otherwise`，它其实是一个 `Bool` 值而不是关键字：

```haskell
otherwise = True
```

## 函数也是数据类型

如果对 `(->)` 进行 `:i` 的话：

```haskell
> :i (->)
data (->) (a :: TYPE q) (b :: TYPE r)   -- Defined in ‘GHC.Prim’
infixr 0 ->
```

会发现函数其实是一种数据类型，并且 `(->)` 是右结合的，这意味着：

```haskell
foo :: a -> a -> a
-- 等价于
foo :: a -> (a -> a)
```

柯里化正是 `(->)` 运算符的右结合性质的结果。

## 函子，可应用函子，还有单子

在对 `(->)` 进行 `:i` 时，会发现几个熟悉的类型类：

```haskell
> :i (->)
instance Functor ((->) r)
instance Applicative ((->) a)
instance Monad ((->) r)
```

函数也实现了 函子，可应用函子和单子，试试怀着好奇心接着看下去吧。

首先是 Functor，Functor 只定义了一个函数：`fmap`，来看看它的类型签名：

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

试着将 `((->) r)` 代入：

```haskell
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
```

将 `(->)` 运算符中置：

```haskell
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

`fmap` 的功能是将 `f a` 中的 a 拿出来，然后应用 `(a -> b)`，最后返回一个 `f b`，想一想要怎么把这里的 `(r -> a)` 拿出来？

要从 `(r -> a)` 中拿出 `a` 就首先需要一个 `r`，但似乎没有任何地方能给我们一个 `r`。不过回想一下 `(->)` 运算符的右结合性质，就可以把最后一个括号消除掉。

```haskell
fmap :: (a -> b) -> (r -> a) -> r -> b
```

第三个参数不就是一个 `r` 吗？

```haskell
fmap :: (a -> b) -> (r -> a) -> r -> b
fmap f a r = f (a r)
```

这里再介绍另一个函数，复合函数 `(.)`：

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

它将两个函数复合，使 `f (g x)` 和 `(f . g) x` 等价。再看看 `fmap` 和 `(.)`，是不是实现十分相似呢？
所以有：

```haskell
fmap :: (a -> b) -> (r -> a) -> r -> b
fmap = (.)
```

接下来是可应用函子，Applicative 定义了两个函数：`pure` 和 `(<*>)`。根据上面的经验，将 `(->) r` 代入并中置运算符：

```haskell
pure :: a -> r -> a
(<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
```

`pure` 的功能是将一个 `a` 包含在一个 Applicative 中，因此 `r` 对 `pure` 来说是毫无用处的，所以有：

```haskell
pure = const
```

`(<*>)` 的功能与 `fmap` 类似，只是需要将 `(r -> a -> b)` 中的 `(a -> b)` 通过 `r` 取出来罢了：

```haskell
(<*>) f g x = f x (g x)
```

最后是单子，Monad 定义了和 Applicative 类似的两个函数：`return` 和 `(>>=)`

```haskell
return :: a -> r -> a
(>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
```

`return` 默认实现是 `pure`，这里不再过多介绍。接下来是 `(>>=)`，与 `(<*>)` 类似，也是通过第三个参数的 `r` 来进行运算的：

```haskell
(>>=) f g x = g (f x) x
```