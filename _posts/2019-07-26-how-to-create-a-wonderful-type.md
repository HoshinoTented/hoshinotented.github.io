---
layout: post
title: Haskell 如何打造爆款数据类型
post: YES!
---

数据类型是 Haskell 中的重要知识之一  
这篇文章讲教你从一无所知到随手打造爆款数据类型  

# 数据类型的定义
数据类型通常用 `data` 和 `newtype` 关键字定义  
接下来，我们首先介绍 `data`  

## Haskell 中的枚举类型
Haskell 中有两种数据类型，一种是 枚举类型，另一种是 构造类型，同时这两种类型也可以互相结合，成为更强大的类型  
首先介绍枚举类型  
枚举类型和 C/C++ 中的 `enum` 类似，就是一个个枚举出这个类型的值  
比如 Haskell 标准库中的 `Bool`  
```haskell
DataType> :i Bool
data Bool = False | True        -- Defined in ‘GHC.Types’
```

`Bool` 有两个值，分别是 `False` 和 `True`  
它们中间被 `|` 分隔开，这个 `|` 便是枚举类型的分隔符  

## 自定义枚举类型
我们照壶画瓢，试着定义一个属于自己的枚举类型  
```haskell
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
```
这看起来很不错，我们定义了一个拥有 7 个值的 Day 类型  
就像在 GHCi 中输入 `True` 一样，我们试着输入 `Mon`:  
```haskell
DataType> Mon

<interactive>:3:1: error:
    ? No instance for (Show Day) arising from a use of ‘print’
    ? In a stmt of an interactive GHCi command: print it
```
它报错了，为什么？  
错误信息表示我们的 `Day` 类型没有实现 `Show` 类型类  
因此不知道如何显示它  
最简单的修复方法是使用 `deriving`  

把我们自定义的数据类型修改成这样  
```haskell
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Show)
```
这样表示让 Haskell 使用默认实现来让我们的 Day 实现 Show  
然后再在 GHCi 中输入 `Mon`  
```
DataType> Mon
Mon
```
GHCi 很成功地输出了 `Mon`  

## Haskell 中的构造类型
刚才除了枚举类型，还说到了构造类型  
构造类型看起来是这样的  
```haskell
-- People Name Age
data People = People String Int deriving (Show)
```

在 GHCi 中输入 `:t People` 看看会发生什么  
```
DataType> :t People
People :: String -> Int -> People
```

它输出了 `People` 函数的类型，这个函数接收 String, Int 最后返回一个 People  
这和我们定义的 `People String Int` 差不多  
试着输入 `People "Hoshino" 4`:  
```
DataType> People "Hoshino" 4
People "Hoshino" 4
```
GHCi 很成功地构造了一个 People 类型的值，并打印了出来  

## 模式匹配
那要如何取出构造类型中的值呢  
首先我们可以使用 **模式匹配**  

```haskell
getName :: People -> String
getName (People name age) = name
```
像构造 People 那样，把 People 解构，这样就是模式匹配  
试着在 GHCi 中输入:  
```
DataType> getName (People "Hoshino" 4)
"Hoshino"
```
函数很正确地取出了 `"Hoshino"`  
但这样也有一个坏处，一旦构造类型内的值非常多，手写就不太现实了  
于是我们可以通过修改类型的定义来做到这一点  
```haskell
data People = People {
    name :: String,
    age :: Int
} deriving (Show)
```

然后在 GHCi 中:  
```
DataType> name (People "Hoshino" 4)
"Hoshino"
```
这样很棒，Haskell 自动帮我们生成了类似于 `getName` 的函数  

## 参数化类型
参数化类型基于构造类型，它额外接收几个 类型参数，使得我们的构造类型可以存放各种类型的数据  
比如 Haskell 标准库中常用的 `Maybe`:  
```haskell
DataType> :i Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Maybe
```
`Maybe` 接收了一个 a 作为类型参数，有一个 `Nothing` 值，和一个 `Just a` 值  
`Maybe` 一般用于处理错误，`Nothing` 代表出错，而 `Just a` 代表成功，并包含了一个值  
试着在 GHCi 中输入:  
```haskell
DataType> Just 1
Just 1
DataType> Nothing
Nothing
```
同样，也可以对其进行模式匹配  
```haskell
isNull :: Maybe a -> Bool
isNull Nothing = True
isNull (Just _) = False
```
GHCi 中  
```haskell
DataType> isNull (Just 1)
False
```

## 自定义参数化类型
试着自己定义一个参数化类型，比如树  
```haskell
data Tree a = Leaf { value :: a }
    | Node { left :: Tree a, value :: a, right :: Tree a }
    deriving (Show)
```  
~~看起来很直观，至少比 C/C++ 强多了~~  
那接下来就可以对我们自定义的 `Tree a` 进行前中后序遍历了  

```haskell
-- ...

mid :: Tree a -> [a]
mid (Leaf v) = [v]
mid (Node left v right) = mid left ++ [v] ++ mid right  

-- ...
```

## 什么是 newtype
`newtype` 可以看做是 data 的简化版  
它定义的数据类型只能有一个枚举值，同时也只能有一个构造值  
与之等价的使用 `data` 定义的数据类型，开销会比 `newtype` 的更大  
`newtype` 的语法和 `data` 相似，看起来是这样的:  
```haskell
-- data QAQ = QAQ Int
newtype QAQ = QAQ Int
```

# 类型别名  
你可以使用 `type` 关键字给类型取一个别名，就像 C 的 `typedef` 和 C++ 的 `using`  
```haskell
type Name = String
type Age = Int

-- data People = People String Int
data People = People Name Age
```
看起来更加直观了

# Haskell 中常用的数据类型  
接下来，让我来介绍一下 Haskell 中常用的数据类型吧

## Int
数字类型是最基础的类型，它的定义。。。  
```haskell
DataType> :i Int
data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in ‘GHC.Types’
```
这都什么啊？  
咳咳。。重点不是这个，让我们来看看 `Int` 的范围吧  
```haskell
DataType> maxBound :: Int
9223372036854775807
DataType> minBound :: Int
-9223372036854775808
```
`maxBound` 和 `minBound` 用于获取类型的最大值和最小值，需要实现 `Bounded` 类型类  
不过这个以后再说，我们会发现 Haskell 的 `Int` 返回比 C 的 `int` 范围大多了  
有的时候我们根本不需要这么大的范围，怎么办呢  
可以导入 `Data.Int` 包，内置了 `Int8`, `Int16`, `Int32` 和 `Int64` 这四种类型  
后面的数字代表占用的位数  

## Word
不！溢出了！我明明用了 `Int64`！！！  
时常有这种情况，但有的时候 `Int64` 的确不够用，怎么办呢  
Haskell 提供了无符号的 `Int` 类型，名为 `Word`  
```haskell
DataType> :m +Data.Word
DataType Data.Word> minBound :: Word
0
```
同样，Word 也有 Word8, Word16, Word32 和 Word64 这四种类型  

## Integer
不！！！又溢出了！！！我明明用了 `Word64`！！！  
如果数据过于刁钻，连 `Word64` 都装不下的时候，就只能出杀手锏了  
**高精度整数**  
`Integer` 是 Haskell 内置的高精度整数，理论上可以存储无限精度的整数  
数字字面量可以作为 `Int` 使用，也可以作为 `Integer` 使用  
比如这样  
```haskell
DataType> :t 1 :: Integer
1 :: Integer :: Integer
```
但 `Integer` 和 `Int` 的互相转换通常是新手的难题  
* `Integer` to `Int`: 使用 `fromInteger` 函数  
    ```haskell
    DataType> :t fromInteger
    fromInteger :: Num a => Integer -> a
    ```
    它接受一个 `Integer` 值，然后返回一个实现了 `Num` 类型类 的类型，而 `Int` 刚好实现了 `Num` 类型类  

* `Int` to `Integer`: 使用 `toInteger` 函数  
    ```haskell
    DataType> :t toInteger
    toInteger :: Integral a => a -> Integer
    ```
    `Int` 也实现了 `Integral` 类型类，这样传入一个 `Int` 值就能返回一个相应的 `Integer` 了

不过，`Integer` 毕竟不是原生类型，效率与原生的 `Int` 和 `Word` 还是有一定差距  

## 列表  
在 GHCi 中输入:  
```haskell
DataType> :i []
data [] a = [] | a : [a]        -- Defined in ‘GHC.Types’
```
看起来有点莫名其妙，这样子呢？
```haskell
data [] a = [] | (:) a ([] a)
```
如果还是不懂的话，让我们自己来实现一个列表  
```haskell
data List a = Empty | Cons a (List a) deriving (Show)
```
这样是不是比较直观了呢  
`Empty` 代表一个空列表  
`Cons a (List a)` 代表把一个 a 值连接在一个 List a 之前  
很容易看出 Haskell 的列表其实是链表实现  

关于 Haskell 的列表定义，要注意的是 `[]` 是 Haskell 内置的列表符号  
所以我们自己是无法定义出名为 [] 的列表的  
而关于 `(:)` 这个构造器名称，我们也是可以定义出类似的构造器  
例如:  
```haskell
-- data List a = Empty | a :> (List a) deriving (Show)
data List a = Empty | (:>) a (List a) deriving (Show)
```
然后就可以 `1 :> Empty` 啦  
不过要注意，`(:)` 也是 Haskell 内置的符号  

## Maybe
这是之前简略介绍过的类型，在其他语言中也有类似的数据类型，比如 Rust 中的 Option，Java8 中的 Optional  
它一般被用来作为 `null` 的替代品，或者用来处理错误  

## Either
这个类型是 `Maybe` 的进阶版，它能够让错误携带错误信息，我们来看看定义:  
```haskell
DataType> :i Either
data Either a b = Left a | Right b      -- Defined in ‘Data.Either’
```
定义十分简单，与 `Maybe` 的差别仅仅只是多了一个类型参数而已  
一般 `Right b` 被视为成功的值，因为 right 也有 “正确” 的意思  
而 `Left a` 也就因此躺枪，被视为了错误  

## String
字符串也是在编程过程中经常接触的一种类型，不过它其实是 `[Char]` 的类型别名  
```haskell
DataType> :i String
type String = [Char]    -- Defined in ‘GHC.Base’
```
因此也可以使用 列表 的所有函数  
但事实上，这种字符串是十分低效的  
Haskell 在 text 库中提供了 Data.Text.Text 类型  
这是一种高效的字符串，不过因为篇幅原因，在这里不过多介绍  
如果有兴趣，可以在[这里](http://hackage.haskell.org/package/text)了解更多关于 Text 类型的内容  

## 函数
是的，函数也是一种数据类型  
```haskell
DataType> :i (->)
data (->) (a :: TYPE q) (b :: TYPE r)   -- Defined in ‘GHC.Prim’
infixr 0 ->
```
`infixr` 是什么？  
这是 Haskell 对运算符优先级的声明语句  
`infixr` 代表运算符是右结合的，`infixl` 则代表左结合  
而 0 代表了运算符的优先级，0 为最高，9 则是最低  
因为 `(->)` 是右结合的，所以也能够说明为什么 `Int -> Int -> Int` 和 `Int -> (Int -> Int)` 等价了  

# 奇怪的共同点
在刚才介绍的这些数据类型中，很容易找到一个共同点  
构造器和类型的名称都不是非小写字母的  
这是因为 Haskell 会把大写开头识别为类型，而小写开头识别为函数  
