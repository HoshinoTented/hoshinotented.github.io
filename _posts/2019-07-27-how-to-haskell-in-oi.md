---
layout: post
title: 如何在 OI 中愉快地 Haskell
post: YES!
---

纯函数式的 Haskell 在 OI 中时常会遇到一些问题  
这篇文章将带你愉快地在 OI 中使用 Haskell  
~~当然是仅限练习，考场上可时没有 Haskell 的~~  

## IO Monad
Haskell 是纯函数式语言，但 OI 中经常涉及读入输出操作，这些都是不纯的，应该怎么办呢  
Haskell 给出了解决方案：`IO Monad`  
