---
layout: post
title: 如何在 OI 中愉快地 Haskell
post: YES!
---

**这篇文章可能需要之前没有介绍过的前置知识，在本文中会粗略介绍**  

纯函数式的 Haskell 在 OI 中时常会遇到一些问题  
这篇文章将带你愉快地在 OI 中使用 Haskell  
~~当然是仅限练习，考场上可是没有 Haskell 的~~  

# 基础操作
一些基础内容可以看 [洛谷日报的 #188 期](https://www.luogu.org/blog/i-love-illya/haskell-ru-men-yu-fa-shou-ce)

## 数值运算
首先讲解对数值的操作  
比如 `+-*/`  
首先是 `(+) (-) (*)`  
```haskell
> :i Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a

  -- ...
```

它们都被定义在了 `Num` 类型类里  
```haskell
> 1 + 2
3
> 1 - 2
-1
> 1 * 2
2
```

接下来是 `(/)`  
但是如果我们看一下 `(/)` 的定义。。。  
```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Real’
infixl 7 /
```
需要 `Fractional` 类型类，我们再看看 `Fractional` 类型类的定义  
```haskell
> :i Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  -- ...
instance Fractional Float -- Defined in ‘GHC.Float’
instance Fractional Double -- Defined in ‘GHC.Float’
```
只有 `Float` 和 `Double` 实现了 `Fractional` 类型类  
这就意味着，我们不能对 `Int` 使用 `(/)` 函数了！  

但我们可以使用 `div`  
```haskell
> :i div
class (Real a, Enum a) => Integral a where
  div :: a -> a -> a
infixl 7 `div`
> :i Integral
class (Real a, Enum a) => Integral a where
  div :: a -> a -> a
instance Integral Int -- Defined in ‘GHC.Real’
```
于是可以这样进行除操作  
```haskell
> div 5 2
2
```
或者将 `div` 函数放在中间  
```haskell
> 5 `div` 2
2
```

最后是取模  
```haskell
> :t mod
mod :: Integral a => a -> a -> a
> 3 `mod` 2
1
```
Haskell 并没有提供 `(%)` 运算符，不过你可以自己定义  
```haskell
> (%) = mod
> 3 % 2
1
```

## 位运算
位运算也是 OI 中很重要的知识  
Haskell 的位运算函数都包含在了 Data.Bits 包中  
```haskell
import Data.Bits
```
使用 `(.&.) (.|.)` 和 `xor` 来进行 且 或 异或 操作
```haskell
> 2 .&. 1
0
> 2 .|. 1
3
> 2 `xor` 1
3
```
使用 `shiftL` 和 `shiftR` 来进行位移  
```haskell
> 1 `shiftL` 9
512
> 512 `shiftR` 9
1
```
你甚至可以定义一个 `(<<)` 运算符  
```haskell
> (<<) = shiftL
> 1 << 9
512
```
不过要注意的是，`(>>)` 被 Monad 使用了，定义 `(>>)` 运算符之前记得先隐藏 Prelude 中的 `(>>)` 运算符  

# 链表
Haskell 内置的列表就是一个链表实现  
```haskell
> :i []
data [] a = [] | a : [a]
```

## 构造列表
可以通过空列表的构造器 `[]` 和 连接列表的构造器 `(:)` 来构造一个列表  
```haskell
> 1:2:3:4:5:[]
[1,2,3,4,5]
```
当然也可以直接 `[1, 2, 3, 4, 5]`  
```haskell
> [1, 2, 3, 4, 5]
[1,2,3,4,5]
```
通过 `[begin..end]` 来构造一个区间  
```haskell
> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```
构造一个等差序列  
```haskell
> [1, 2..10]
[1,2,3,4,5,6,7,8,9,10]
> [1, 3..10]
[1,3,5,7,9]
```
使用列表生成器  
```haskell
> [x | x <- [1..5], even x]
[2,4]
> [y | y <- [1..5], even y]
[2,4]
> [x + y | x <- [1..5], y <- [1..5], even x && even y]
[4,6,6,8]
```

## 列表常用函数
这里将介绍部分列表常用函数(同时也是对 [洛谷日报的 #188 期](https://www.luogu.org/blog/i-love-illya/haskell-ru-men-yu-fa-shou-ce) 的补充)

名称(及函数签名) | 模块 | 作用
--------------|------|-------
head :: [a] -> a | Prelude | 取出列表的首元素
last :: [a] -> a | Prelude | 取出列表的尾元素
init :: [a] -> [a] | Prelude | 取出除了最后一个元素以外的元素
tail :: [a] -> [a] | Prelude | 取出除了第一个元素以外的元素
take :: Int -> [a] -> [a] | Prelude | 获取列表前 n 个元素
drop :: Int -> [a] -> [a] | Prelude | 删除列表前 n 个元素
(++) :: [a] -> [a] -> [a] | Prelude | 连接两个列表
(!!) :: [a] -> Int -> a | Prelude | 随机访问
elem :: (Foldable t, Eq a) => a -> t a -> Bool | Prelude | 检查目标元素是否存在于列表中
length :: Foldable t => t a -> Int | Prelude | 返回列表长度
map :: (a -> b) -> [a] -> [b] | Prelude | 对列表的每个元素应用函数，并返回函数结果的列表
filter :: (a -> Bool) -> [a] -> [a] | Prelude | 过滤列表中元素
reverse :: [a] -> [a] | Prelude | 翻转列表
sort :: Ord a => [a] -> [a] | Data.List | 对列表排序(从小到大)
sortBy :: (a -> a -> Ordering) -> [a] -> [a] | Data.List | 根据传入的比较函数对列表排序

# IO Monad
Haskell 是纯函数式语言，但 OI 中经常涉及读入输出操作，这些都是不纯的，应该怎么办呢  
Haskell 给出了解决方案：`IO Monad`  

## 浅谈 Monad
要理解 `IO Monad`，首先就要知道什么是 `Monad`  
那什么是 `Monad` 呢？  
让我们看看 `Monad` 的定义:  
```haskell
> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
        -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
```
最重要的是 `(>>=)` 和 `return`  
`return` 用于接收一个值，并把这个值包装进 `Monad` 里  
`(>>=)` 用于取出值，并把值应用到传入的函数，然后返回一个新的 Monad  
但无论是 `return` 还是 `(>>=)`  
它们都不能把值从 `Monad` 中取出并返回  
这也就意味着，如果一个值进入了 `Monad`，就再也无法通过 `Monad` 提供的操作出来了。。。  
而 `Monad` 的这个特性，恰好也是 `IO Monad` 的本质  
一旦函数中使用了 `IO Monad`，那么这个函数的返回值也一定会带有 `IO Monad`  
所以也就能区分纯函数和不纯函数了  

## 何为 (>>=)
`(>>=)` 读作 bind  
bind 是 Monad 中最重要的操作，Monad 定义中的 `{-# MINIMAL (>>=) #-}` 代表了实现 Monad 至少要实现 `(>>=)` 这个函数  
我们首先看看 `(>>=)` 的函数签名  
```haskell
> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```
它意为 接收一个 `Monad`，进行一次运算，然后再返回一个 `Monad`  
但要如何使用 `(>>=)` 呢  
让我们先看看 `putStrLn` (用于输出字符串，下面会提到)  
```haskell
> :i putStrLn
putStrLn :: String -> IO ()
```
它接收一个 `String`，然后返回一个 `IO ()`  
不过 `IO` 这个类型实现了 `Monad`  
我们这样调用 `putStrLn`  
```haskell
> putStrLn "qwq"
qwq
```
但我们想要在一条语句里调用两次 `putStrLn`  
要怎么做呢？  
答案是: 使用 `(>>=)`  
首先，我们先分别调用两次 `putStrLn`  
```haskell
> putStrLn "one"
one
> putStrLn "two"
two
```
然后使用 `(>>=)` 把这两个操作连接起来  
```haskell
> putStrLn "one" >>= \_ -> putStrLn "two"
one
two
```
或者直接使用 `(>>)`  
```haskell
> putStrLn "one" >> putStrLn "two"
one
two
```
还可以调用更多次的 `putStrLn`  
```haskell
> :{
| printN :: Int -> String -> IO ()
| printN 0 _ = return ()
| printN n str = putStrLn str >> printN (n - 1) str
| :}
> printN 5 "qwq"
qwq
qwq
qwq
qwq
qwq
```
Monad 的 `(>>=)` 可以用来进行连续的计算  
也使得 纯函数式的 Haskell 可以以命令式的方式来编写程序  

Haskell 还为 `(>>=)` 提供了一个语法糖: `do`  
比如:  
```haskell
main :: IO ()
main = getLine >>= \s ->
    putStrLn s
```
等价于  
```haskell
main :: IO ()
main = do
    s <- getLine
    putStrLn s
```
这样就显得更加命令式了  
`IO Monad` 的部分极为重要，无法掌握这部分知识代表着无法让 Haskell 程序与系统交互，从而也无法解决题目

## 输出
你可以使用这两个函数来输出: `putStrLn` 和 `print`  
```haskell
> putStrLn "qwq"
qwq
> print "qwq"
"qwq"
```
会发现两者的输出不太一样，但如果我们看看 [print 的实现](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/System.IO.html#print)  
会发现它其实是:  
```haskell
print x = putStrLn (show x)
```
或者
```haskell
print = putStrLn . show
```
这两者是等价的  
首先通过 `show` 函数把值转化为 `String`，再使用 `putStrLn` 输出  

## 输入
可以使用 `getChar` 或者 `getLine` 来读入一个字符或者一整行  
```haskell
> getChar
1
'1'
> getLine
qwq
"qwq"
```
使用 `words` 来根据空格分割  
```haskell
> words <$> getLine
1 2 3
["1","2","3"]
```
使用 `read` 将字符串转化为 `Int`  
```haskell
> map read . words <$> getLine :: IO [Int]
1 2 3
[1, 2, 3]
```

## Text
Haskell 自带的 `String` 效率低下，这使得我们需要使用更高效的 `String`，即 Data.Text  
```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
```
`Data.Text` 是 text 本体  
`Data.Text.IO` 提供了关于 `Text` 的 IO 操作  
比如  
```haskell
> map (read . T.unpack) . T.words <$> TIO.getLine :: IO [Int]
1 2 3
[1, 2, 3]
```

## 读入优化
使用 `read` 来转换数据实在是太慢了，这使得我们需要自己写一个转换器  
```haskell
-- Parse Int :: Count -> Source -> Result
parseInt :: Int -> String -> Int
parseInt n [] = n
parseInt n (char:str) = parseInt (n * 10 + (ord char - ord '0')) str

main :: IO ()
main = do
    xs <- map (parseInt 0 . T.unpack) . T.words <$> TIO.getLine :: IO [Int]
    print xs
```

这样，我们就完成了 Haskell 中的读入优化  

# 数组
列表是链表实现，OI 中常常需要随机访问，所以列表并不胜任这份工作  
虽然 Haskell 有内置的 Data.Array，但性能和易用性方面还是太差了  
这里介绍 [vector 库](http://hackage.haskell.org/package/vector)  

## 不可变 Vector
首先导入 Data.Vector.Unboxed （Data.Vector 也是可以的，但 Unboxed 版本的 Vector 对实现了 Unbox 类型类的类型 (例如 Int) 有空间和时间优化，适合 OI 使用）  
```haskell
import qualified Data.Vector.Unboxed as V
```
什么是 `qualified`？  
`qualified` 关键字代表这个包不直接引入到全局命名空间里  
而 `as` 则是将包重命名  
比如这里重命名为 `V`  

### 构造 Vector
首先，你可以创建一个空 `Vector`  
```haskell
> :i V.empty
V.empty :: V.Unbox a => V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
或者创建只有一个元素的 `Vector`  
```haskell
> :i V.singleton
V.singleton :: V.Unbox a => a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
时间复杂度都是 $O(1)$  

创建一个带有 n 个 i 的 `Vector`  
```haskell
> :i V.replicate
V.replicate :: V.Unbox a => Int -> a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
可以像这样使用  
```haskell
> V.replicate 5 1 :: V.Vector Int
[1,1,1,1,1]
```
或者创建带有 n 个元素的 `Vector`
```haskell
> :i V.generate
V.generate :: V.Unbox a => Int -> (Int -> a) -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
`generate` 接收一个 `Vector` 长度，和一个 `(Int -> a)` 的函数，意思是传入一个数组下标，返回一个数组值  
最后返回一个生成好的 `Vector`  

或者你可以选择递推的方式构造出一个 `Vector`  
```haskell
> :i V.iterateN
V.iterateN :: V.Unbox a => Int -> (a -> a) -> a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
`iterateN` 接收一个 `Vector` 长度， 一个 `(a -> a)` 的函数，代表接收上一个值，返回当前值  
然后再接收一个初始值，最后返回构造好的 `Vector`  
比如我们使用 `iterateN` 构造一个斐波那契数列  
```haskell
> (V.iterateN 10 (\(a, b) -> (b, a + b)) (0, 1)) :: V.Vector (Int, Int)
[(0,1),(1,1),(1,2),(2,3),(3,5),(5,8),(8,13),(13,21),(21,34),(34,55)]
```
发现元组的第一项是 0, 1, 1, 2, 3, 5, 8, 13 ...  
这的确是斐波那契数列  
你还可以把一个 Haskell 列表转化为 `Vector`  
```haskell
> :t V.fromList
V.fromList :: V.Unbox a => [a] -> V.Vector a
> V.fromList [1, 1, 4, 5, 1, 4] :: V.Vector Int
[1,1,4,5,1,4]
```
这些函数的时间复杂度是 $O(n)$  

### 访问 Vector
我们导入 Data.Vector.Unboxed 中的访问运算符，可以简化我们的代码  
```haskell
import Data.Vector.Unboxed ((!))
```
然后就可以使用这种方式来随机访问 `Vector` 了  
```haskell
> :i (!)
(!) :: V.Unbox a => V.Vector a -> Int -> a
        -- Defined in ‘Data.Vector.Unboxed’
> v = V.generate 5 id
> v ! 3
3
```
时间复杂度是 $O(1)$  

### 连接 Vector
解决问题的时候，避免不了往 `Vector` 中添加值  
使用 `cons` 把 值 添加在 `Vector` 的前端  
```haskell
> :i V.cons
V.cons :: V.Unbox a => a -> V.Vector a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
> V.cons 1 (V.fromList [2, 3, 4]) :: V.Vector Int
[1,2,3,4]
```
或者使用 `snoc` (这不就是把 `cons` 反过来写了吗！) 把 值 添加在 `Vector` 后端  
```haskell
Prelude V Data.Vector.Unboxed> :i V.snoc
V.snoc :: V.Unbox a => V.Vector a -> a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
> V.snoc (V.fromList [1, 2, 3]) 4 :: V.Vector Int
[1,2,3,4]
```
时间复杂度都是 $O(n)$  
使用 `(++)` 来连接两个 `Vector`  
不过使用 `(++)` 之前还需要隐藏 Haskell 自带的 `(++)`  
```haskell
import Prelude hiding ((++))
```
然后导入 `Vector` 的 `(++)`  
```haskell
import Data.Vector.Unboxed ((++))
```
然后连接两个 `Vector`  
```haskell
> :i (++)
(++) :: V.Unbox a => V.Vector a -> V.Vector a -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
infixr 5 ++
> V.fromList [1, 2, 3] ++ V.fromList [4, 5, 6] :: V.Vector Int
[1,2,3,4,5,6]
```
时间复杂度是 $O(n + m)$

### 修改 Vector
不能被修改的 `Vector` 不是好 `Vector`！  
可以通过 `(//)` 运算符来实现修改值  
```haskell
> import Data.Vector.Unboxed ((//))
> :i (//)
(//) :: V.Unbox a => V.Vector a -> [(Int, a)] -> V.Vector a
        -- Defined in ‘Data.Vector.Unboxed’
```
接收一个被修改的 `Vector`，和一个 `(下标, 新值)` 的列表，最后返回一个更新完的 `Vector`  
不过要注意的是，原本的 `Vector` 并不会被修改  
所以复杂度是 $O(m + n)$，$m$ 是 `Vector` 的长度，$n$ 是更新列表的长度  
```haskell
> V.fromList [1, 2, 3, 4] // [(1, 5)] :: V.Vector Int
[1,5,3,4]
```

## 可变 Vector
不可变 `Vector` 有很多缺点，比如在 oi 中十分致命的性能问题  
在频繁修改的时候会产生大量拷贝，使得我们的代码 TLE  
因此就要选择 OIer 们最熟悉的 `可变 Vector`  
但可变 `Vector` 因为是可变的，所以就变得不纯了  
这里又要请到 `IO Monad` 了  

首先导入 Data.Vector.Unboxed 的 Mutable 版本  
```haskell
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed.Mutable (IOVector)
```

### 构造 Vector
可变 `Vector` 的构造方式就没有 不可变 `Vector` 那么多样了，只提供了 `new` 和 `repilicate`  
```haskell
> :i MV.new
MV.new ::
  (Control.Monad.Primitive.PrimMonad m, MV.Unbox a) =>
  Int -> m (MV.MVector (Control.Monad.Primitive.PrimState m) a)
        -- Defined in ‘Data.Vector.Unboxed.Mutable’
```
函数签名有些复杂，其实就是接收一个长度 $n$，并返回一个长度为 $n$ 的 `Vector`  
```haskell
main :: IO ()
main = do
    vec <- MV.new 5 :: IO (IOVector Int)

    return ()
```

### 访问 Vector
可变 `Vector` 可以使用 `read` 函数来进行随机访问  
```haskell
main :: IO ()
main = do
    vec <- MV.new 5 :: IO (IOVector Int)

    v <- MV.read vec 0      -- 读取 vec 中下标为 0 的元素
    print v

    return ()
```
输出了 0  
时间复杂度是 $O(1)$

### 写入 Vector
可变 `Vector` 可以使用 `write` 函数来进行随机写入  
```haskell
main :: IO ()
main = do
    vec <- MV.new 5 :: IO (IOVector Int)

    MV.write vec 0 1        -- 对 vec 中下标为 0 的元素赋值 1
    v <- MV.read vec 0      -- 读取 vec 中下标为 0 的元素
    print v

    return ()
```
输出了 1  
时间复杂度是 $O(1)$

### 修改 Vector
可变 `Vector` 可以使用 `modify` 函数来进行修改  
```haskell
main :: IO ()
main = do
    vec <- MV.new 5 :: IO (IOVector Int)

    MV.modify vec (+1) 0    -- 对 vec 中下标为 0 的元素应用 (+1) 函数
    v <- MV.read vec 0      -- 读取 vec 中下标为 0 的元素
    print v

    return ()
```
输出了 1
时间复杂度是 $O(1)$

### 扩容 Vector
如果大小不够用了，可以使用 `grow` 函数来扩大 `Vector` 的容量  
```haskell
main :: IO ()
main = do
    vec <- MV.new 5 :: IO (IOVector Int)

    print $ MV.length vec
    vec <- MV.grow vec 1            -- 把 vec 扩大 1，然后返回
    print $ MV.length vec

    return ()
```
输出 5 和 6  

# 变量
不！！！没有变量我要死了。。。  
可能很多 OIer 都会这样子  
虽然 Haskell 中没有变量，但提供了一个数据结构  
可以实现类似变量的操作  
当然，依然使用了 `IO Monad`  

首先导入 `Data.IORef` 包
```haskell
import Data.IORef
```

## 创建 IORef
使用 `newIORef` 来创建一个变量  
```haskell
-- newIORef :: a -> IO (IORef a)
main :: IO ()
main = do
    v <- newIORef 1

    return ()
```

## 读取 IORef
使用 `readIORef` 来读取变量中的值  
```haskell
-- readIORef :: IORef a -> IO a
main :: IO ()
main = do
    v <- newIORef 1
    readIORef v >>= print

    return ()
```

## 写入 IORef
使用 `writeIORef` 来向变量中写入值  
```haskell
-- writeIORef :: IORef a -> a -> IO ()
main :: IO ()
main = do
    v <- newIORef 1
    writeIORef v 2
    readIORef v >>= print

    return ()
```

## 修改 IORef
使用 `modifyIORef` 来修改变量中的值
相当于 `readIORef` 和 `writeIORef` 的结合  
```haskell
-- modifyIORef :: IORef a -> (a -> a) -> IO ()
main :: IO ()
main = do
    v <- newIORef 1
    modifyIORef v (+1)
    readIORef v >>= print

    return ()
```

虽然 Haskell 提供了 `IORef`，但我并不支持滥用 `IORef` 的行为 ~~(可以无视我说的话)~~  

# 映射表
映射表也是在 OI 中常用的一种数据结构  
映射表被包含在 [container 库](http://hackage.haskell.org/package/containers) 中，在 Data.Map 包下  
由于部分函数命名冲突，需要使用 `qualified`  
```haskell
import qualified Data.Map as M
```

## 构造映射表
可以使用 `empty` 来构造一个空表  
`singleton` 来构造包含一个映射的表  
`fromList` 从 `(键, 值)` 映射的列表中构造映射表  
```haskell
> M.empty
fromList []
> M.singleton 1 2
fromlist [(1,2)]
> M.fromList [(1, 2)]
fromlist [(1,2)]
```

## 添加映射
可以使用 `insert` 函数来添加映射  
```haskell
> :i M.insert
M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
        -- Defined in ‘Data.Map.Internal’
> M.insert 1 2 M.empty
fromList [(1,2)]
```

## 访问映射表
使用 `(!)` 或者 `(!?)` 来访问映射表  
```haskell
> import Data.Map ((!), (!?))
> :i (!) (!?)
(!) :: Ord k => M.Map k a -> k -> a
        -- Defined in ‘Data.Map.Internal’
(!?) :: Ord k => M.Map k a -> k -> Maybe a
        -- Defined in ‘Data.Map.Internal’
> M.empty ! 1
*** Exception: Map.!: given key is not an element in the map
CallStack (from HasCallStack):
  error, called at libraries\\containers\\Data\\Map\\Internal.hs:610:17 in containers-0.6.0.1:Data.Map.Internal
> M.empty !? 1
Nothing
```
使用 `(!)` 访问映射表，如果映射不存在就会抛出一个错误  
而 `(!?)` 则是返回一个 `Maybe` 类型  

# 栈
栈可以使用 Haskell 的原生列表实现  
```haskell
pop :: [a] -> ([a], a)
pop (x:xs) = (xs, x)

push :: a -> [a] -> [a]
push = (:)

top :: [a] -> a
top = head
```
还可以使用 State Monad 来更优雅地实现，但此处不过多介绍  

# 队列
队列可以使用两个栈的方法来模拟，这里提供我自己实现的队列  
```haskell
--   Queue 输出端 输入端
data Queue a = Queue [a] [a]

instance (Show a) => Show (Queue a) where
    show (Queue o i) = show $ o ++ reverse i

front :: Queue a -> a
front (Queue [] xs) = last xs
front (Queue xs _) = head xs

push :: Queue a -> a -> Queue a
push (Queue o i) v = Queue o (v:i)

pop :: Queue a -> Queue a
pop (Queue [] []) = error "empty queue"
pop (Queue [] xs) = pop $ Queue (reverse xs) []
pop (Queue (_:xs) ys) = Queue xs ys

fromList :: [a] -> Queue a
fromList xs = Queue xs []
```

# 邪门优化
## 严格求值
Haskell 是惰性的，但惰性求值会影响尾递归的优化  
所以我们需要添加一个扩展，使得 Haskell 严格求值  
在文件开头添加这样一句话:  
```haskell
{-# LANGUAGE Strict #-}
```

## 吸氧
Haskell 也有 O2 优化，在文件开头添加这样一句话:  
```haskell
{-# OPTIONS_GHC -O2 #-}
```

# Hoogle
Hoogle 是 Haskell 生涯中很重要的工具  
[Hoogle](https://hoogle.haskell.org/) 是 Haskell 官方的函数文档  
你可以在这里面查找函数的签名和用法，包括源代码实现  
或者查找各种各样的依赖库  

# 更多
如果对文章内容有疑惑~~，或者想喷我菜的~~，都可以在评论区留言，我会尽可能回复  

---------------------------
参考: [并查集讨论](https://www.luogu.org/discuss/show/125819)