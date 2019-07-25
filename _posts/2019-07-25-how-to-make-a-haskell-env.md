---
layout: post
title: Haskell 环境搭建指南
post: YES!
---

Haskell 是一款 纯函数式 的语言，我个人也热衷于 Haskell  
下面就来介绍一下如何搭建 Haskell 的环境

# GHC
ghc 是 Haskell 的编译器，就像 gcc 和 C 关系一样  
接下来就介绍如何在各个平台安装 ghc  

## Linux
以 Ubuntu(APT) 为例，需要在命令行输入以下代码:  
```bash
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-<version> ghc-<version>
```

其中，cabal 是用于管理 Haskell 依赖库的程序，而 ghc 则是 Haskell 的编译器

更详细的内容都在[这里](https://www.haskell.org/downloads/linux/)

## Windows
首先从[这里](https://www.haskell.org/platform/windows.html)下载 GHC，然后安装

# Stack

但安装原版 ghc 并不是在我们 haskell 之旅中的一个好的选择  
比如原版 ghc 在更新时时常会出现一些问题，而且 cabal 的包管理机制也并不是很完善  
这个时候，亲身体验了这些风霜的 haskeller 们站了出来，于是便有了 stack  

## 如何安装 Stack
首先要找到 stack 的官方网站: [https://haskellstack.org](https://docs.haskellstack.org/en/stable/README/)

![](https://cdn.luogu.org/upload/pic/65723.png)

### Linux/MacOS 用户
只需要在控制台里输入  
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
就能够安装 stack

### Windows 用户
需要在官网下载安装包（即上图的 `Windows 64-bit Installer`，这个过程可能会比较缓慢，可以通过虚拟专用网络加速这个过程），随后双击打开  
![](https://cdn.luogu.org/upload/pic/65707.png)  
自行选择安装路径，之后点击 `Next`  
![](https://cdn.luogu.org/upload/pic/65708.png)  
下面的两个选项最好勾选，可以自动添加环境变量，少去了自己配置的麻烦  
再点 `Install` 就可以完成安装了

## Stack 初始化

安装完 stack 后，打开命令行，输入以下指令

```bash
stack setup
```

用于初始化 stack，可能需要一点时间  

## 为 Stack 修改源

stack 的官方源在中国大陆可能会非常慢，因此我们需要修改 stack 的源  
请根据你的系统来找到相应的 stack 目录:  
* Windows: `C:\sr`
* Linux/MacOS: `~/.stack`

在 stack 目录下找到名为 `config.yml` (如果没有就自己新建一个) 的配置文件，输入以下内容：  

```yml
# hackage
package-indices:
  - name: Tsinghua
    download-prefix: http://mirrors.tuna.tsinghua.edu.cn/hackage/package/
    http: http://mirrors.tuna.tsinghua.edu.cn/hackage/00-index.tar.gz

# stackage
setup-info: "http://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"
urls:
  latest-snapshot: http://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json
  lts-build-plans: http://mirrors.tuna.tsinghua.edu.cn/stackage/lts-haskell/
  nightly-build-plans: http://mirrors.tuna.tsinghua.edu.cn/stackage/stackage-nightly/
```

这里使用的是[清华大学](https://mirrors.tuna.tsinghua.edu.cn/)的镜像  
之后，就可以愉快地初始化 stack 了  

# Stack 安装依赖库  
stack 安装依赖库十分简单，只需要:  
```bash
stack install <libaray name>
```
之后稍加等待，就能安装依赖库了  

# GHCi

ghci 是 Haskell 的交互式命令行，具有强大的功能  
可以通过 `stack ghci` 进入 ghci  
在 ghci 中输入以下内容  
```haskell
putStrLn "hoshino so cute!"
```

会输出  
```plain
hoshino so cute!
```

当然，你可以通过 `Tab` 来快速补全

ghci 还提供了一些内置命令，这里列举出常用的

* `:type` 或 `:t`  
    输出目标的类型  
    例如 `:t foldr`  
    输出 `foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`
* `:info` 或 `:i`  
    输出目标的详细内容  
    例如 `:t foldr`  
    输出  
    ```haskell
    class Foldable (t :: * -> *) where
      ...
      foldr :: (a -> b -> b) -> b -> t a -> b
      ...
            -- Defined in ‘Data.Foldable’
    ```
* `:kind` 或 `:k`  
    输出目标的 `kind`  
    例如 `:k Functor`  
    输出 `Functor :: (* -> *) -> Constraint`  

* `:set`  
    设置一些内置功能  
    `:set +t` 可以在每个值计算完后输出它的类型  
    `:set +s` 可以在每个值计算完后输出所使用的时间和空间  
* `:load` 或 `:l`  
    载入目标 Haskell 文件  
    例如: `:l hello.hs`  
    之后就可以使用 `hello.hs` 中提供的函数了  

* `:reload` 或 `:r`  
    重新载入文件  
    可以快速重新载入，就不需要重新 `:l` 了

* `:module` 或 `:m`  
    导入库  
    例如 `:m +Data.List` 可以导入 `Data.List` 库  
    `:m -Data.List` 可以取消导入  
    `:m Data.List` 会取消导入其他库，然后再导入 `Data.List` 库

* `:quit` 或 `:q`  
    退出 ghci

# IDE 环境

编写程序，IDE 当然是能够提高开发效率的一环，接下来就介绍几个常用 IDE 的 Haskell 支持

## VSCode
`VSCode` 有 依赖官方开发的 `haskell-ide-engine` 的 `haskell language server` 和 `dramforever` 开发的 `Simple GHC Integration`  
`haskell language server` 安装麻烦，而且内存占用也比较大，但功能较为强大  
`Simple GHC Integration` 一键式插件，新手友好，功能完善

## IntelliJ
`IntelliJ IDEA` 有 `intellij haskell`  
个人觉得不太好，所以不多加描述  

# Stack Project
可以通过 `stack new <project name>` 来新建一个项目  
随后可以看到这样的项目结构:  
```plain
.
├── app                 // 程序代码目录
│   └── Main.hs
├── ChangeLog.md
├── LICENSE
├── my-project.cabal
├── package.yaml        // 包配置文件
├── README.md
├── Setup.hs
├── src                 // 库代码目录
│   └── Lib.hs
├── stack.yaml          // stack 配置文件
└── test                // 测试代码目录
    └── Spec.hs
```
在 `Lib.hs` 中写入以下代码（代码来自[我的博客](https://blog.hoshino9.org)）:  
```haskell
module Lib (mergeSort) where

import Data.List

-- 归并排序 :: 数组 -> 排序后的数组
mergeSort :: (Eq a, Ord a) => [a] -> [a]
mergeSort (x:[]) = [x]      -- 如果是 只有一个元素的数组, 就返回这个数组
mergeSort xs = merge (mergeSort $ fst subs) (mergeSort $ snd subs)      -- 把 (被归并排序过的 前半部分的数组) 和 (被归并排序过的 后半部分的数组) 进行排序合并
  where
    mid = length xs `div` 2     -- 中心的下标
    subs = splitAt mid xs       -- 把数组分割成两个接近等份(奇数数量情况)或等份(偶数数量情况)的数组

-- 排序并合并两个数组 :: 数组 -> 另一个数组 -> 合并后的数组
merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge [] ys = ys    -- 如果有任意一个数组为空, 则返回非空的那个数组
merge xs [] = xs
merge (x:xs) (y:ys) = if
    x < y then x : merge xs (y:ys)      -- 如果 第一个数组的首元素小于 第二个数组的首元素, 则 排序剩下的两个数组, 并将 第一个数组的首元素添加在排序后的数组之前
    else y : merge (x:xs) ys            -- 这里跟上面差不多啦
```

然后在 `Main.hs` 中写入以下代码，来调用 `src/Lib.hs` 中的函数：  
```haskell
module Main where

import Lib

main :: IO ()
main = print $ mergeSort [5, 2, 1, 4, 3]
```

最后，在项目根目录下  
在命令行中输入以下内容:  
```bash
stack build
```
等待之后，就成功编译这个项目了  
然后通过  
```bash
stack exec <project-name>-exe
```
来运行编译好的程序  
会看到输出: `[1, 2, 3, 4, 5]`  

如果在项目目录下运行 `stack ghci`  
还会发现自动编译并导入了所有的文件  

# 结束语
后续我还会带来关于 Haskell 的更多内容  
如果期待的话，可以关注我  
