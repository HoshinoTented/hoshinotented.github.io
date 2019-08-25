---
layout: post
title: 浅谈 GADT 与 Dependent Type
post: YES!
---

这篇文章将会带你接触 GADT 和 Dependent Type 的奇妙世界  

## 何为 GADTs

GADT 意为广义代数数据类型（**G**eneralized **A**lgebraic **D**ata **T**ype）

那 GADT 和普通的 代数数据类型 有什么不同呢？
我们来看看下面这段代码，这段代码用了一个代数数据类型来表示一个语法树：

### 不安全的语法树

```haskell
data Expr = ILit Int
    | BLit Bool
    | Add Expr Expr
```

我们可以用 `Add (ILit 1) (ILit 2)` 来表示 `1 + 2`，但不知道大家有没有发现一个弊端，
这种定义并不能防止 `Add (BLit True) (ILit 1)` 的情况出现，`Add` 只能作用于数字！

### 看起来安全的语法树

这个时候，我们可以给 `Expr` 添加上一个类型参数

```haskell
data Expr a = ILit Int
    | BLit Bool
    | Add (Expr a) (Expr a)
```

会发现 `Expr a` 并没有包含任何 `a` 的值，因此这个 `a` 被称为 **幻影类型**

这样，我们就可以提供一个限制类型版本的 `add` ：

```haskell
add :: Expr Int -> Expr Int -> Expr Int
add = Add
```

再给 `ILit` 和 `BLit` 提供构造函数：

```haskell
ilit :: Int -> Expr Int
ilit = ILit

blit :: Bool -> Expr Bool
blit = BLit
```

但我们再尝试类似于 `Add (BLit True) (ILit 1)` 的代码 `add (blit True) (ilit 1)` 时，
编译器报错了，这正是我们想要的！

### 安全的语法树

**需要在文件开头加上 `{-# LANGUAGE GADTs #-}`**

使用 GADT，我们就能得到安全的语法树，思考以下代码：

```haskell
data Expr a where
    ILit :: Int -> Expr Int
    BLit :: Bool -> Expr Bool
    Add  :: Expr Int -> Expr Int -> Expr Int
```

GADT 做了什么？似乎现在还看不太出来，试着在 GHCi 里查看 ILit 的类型：

```haskell
> :t ILit
ILit :: Int -> Expr Int
```

我们发现 ILit 的返回值变成了 `Expr Int`，而不再是之前的 `Expr a`！

这意味着 GADT 可以指定类型参数的准确类型，而不是模糊不清的 `a`

## 如何 Dependent Type

思考一下，如果我们试图对一个空列表进行 `head` 操作，会发生什么？

### 不安全的列表

```haskell
data List a = Empty | Cons a (List a)

head' :: List a -> a
head' Empty = undefined -- what should it do?
head' (Cons v _) = v
```

答案当然是报错，或者返回一个 `Maybe` 类型，但如果能在编译期就解决这个问题，那该有多好啊

### 安全的列表

使用 GADT，我们可以把长度信息附加在 List 上，
就像在 C++ 里面的 `std::array` 使用了 `template` 一样  

我们先用类型定义自然数

```haskell
data Z      -- 类型 Z 表示 0
data S n    -- 类型 S n 表示 n 的后继，比如 S Z 代表 0 的后继，即 1
```

定义一个 GADT 风格的列表

```haskell
data List a where
    Empty :: List a
    Cons  :: a -> List a -> List a
```

给列表加上长度信息

```haskell
data List n a where
    Empty :: List Z a
    Cons  :: a -> List l a -> List (S l) a
```

定义一个安全的 `head`

```haskell
head'' :: List (S n) a -> a
head'' (Cons v _) = v
```

当我们运行 `head'' (Cons 1 Empty)` 的时候会返回 1，
但如果是 `head'' Empty` 呢？

编译器会报错，换言之，编译器帮我们把错误在发生前就消灭了

## 类型的类型？

之前定义的 `Z` 和 `S n` 分别都是两个不同的类型，如果我们想要一个 类型的类型 呢？
这听起来好像有点奇怪，但 Haskell 的确能做到：

首先应用 `DataKinds` 扩展：

```haskell
{-# LANGUAGE DataKinds #-}
```

然后重新定义 `Z` 和 `S n`  

```haskell
data Nat = Z | S n
```

这和普通的类型有什么区别？我们看看 Z 和 S 的 **kind**  

```haskell
> :k Z
Z :: Nat
> :k S
S :: Nat -> Nat
```

这意味着什么？`Z` 的 kind 是 `Nat`，`S` 的 kind 是 `Nat -> Nat`。
但事实上，GHC 对 `Z` 和 `S` 进行了隐式转换，真正的 `Z` 应该是 `'Z` **类型**。
所以应该是，**类型** `'Z` 的类型是 `Nat`，**类型** `'S` 的类型是 `Nat -> Nat`。

这就是类型的类型 与 DataKinds

## 对类型进行运算！

思考一下，如果想要连接两个列表，要怎么做呢？

```haskell
concat :: List ?? a -> List ?? a -> List ?? a
```

似乎还思考不出类型签名，没关系，让我们来看另一个东西：`TypeFamilies`，一个能让类型进行运算的扩展。

我们可以定义 `Nat` 类型的加法运算：

```haskell
type family   (Add (a :: Nat) (b :: Nat)) :: Nat
type instance (Add Z b) = b
type instance (Add (S a) b) = S (Add a b)
```

加法？连接两个列表结果的长度不就需要用到加法吗！

```haskell
concat :: List m a -> List n a -> List (Add m n) a
concat = ... -- 留给读者自己思考
```

## 结尾

这里推荐一道 Dependent Type 的练习题：[Codewars-Singletons](https://www.codewars.com/kata/singletons)

如果有讲错的，有建议 ~~或者想喷我菜的~~ 都可以在留言区评论

-------------
参考：
* https://en.wikibooks.org/wiki/Haskell/GADT
* https://colliot.me/zh/2017/11/what-is-gadt-in-haskell/

感谢：
* [ice1000](https://ice1000.org)