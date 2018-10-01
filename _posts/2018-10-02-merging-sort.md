---
layout: post
title: 星野的归并旅程
post: YES!
---

# 写在前面
前几天(好久了吧。。。)突然觉得星野好菜  
只会冒泡排序和插入排序  
最后, 本小姐下定决心  
打算学习 快速排序  
结果被群友安利去学了 归并  
QAQ  
不过群友安利的也是对的  
学会了归并, 快排就会了一半了  
参考资料: [可视化算法-排序](https://visualgo.net/zh/sorting)  

# 归并排序
星野认为, 归并排序的核心是 **将两个有序的数组合并成一个有序的数组**~~(坐等被喷)~~  
那, 如何合并呢  
本小姐在这里摆上一张 **《大话数据结构》** 里的图片  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/merging_sort.jpg?raw=true)  
这些元素呢, 两两合并成了一个 `有序的数组`  
接着这些有序的数组又两两合并为一个 `有序的数组`  
到最后, 我们获得到的, 就是一个 `有序的数组`  
如果还不理解的话~~(怎么可能理解啊, 讲的太死板了呀)~~  
星野就再贴上 [`visualgo`](https://visualgo.net/zh/sorting) 的图片  
首先, 把一个数组分成单个元素  
这些单个元素, 可以被看成是有序的数组, 只不过里面只有一个元素而已  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_partition.png?raw=true)
接着, 把 第一个数组(红色) 和 第二个数组(淡橙色) 合并  
首先, 把<span style="color: #FF9696;">红色数组</span>的第一个元素和<span style="color: #FFD696;">淡橙色数组</span>的第一个元素进行比较, 把较小的移动到<span style="color: #FF0000;">新数组</span>里  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_merge0.png?raw=true)  
接着, 发现<span style="color: #FF9696;">红色数组</span>已经没有元素了, 那么就将<span style="color: #FFD696;">淡橙色数组</span>里的元素全部移动到<span style="color: #FF0000;">新数组</span>里  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_merge1.png?raw=true)  
最后将<span style="color: #FF0000;">新数组</span>的所有元素复制回去  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_merge2_copy.png?raw=true)  
这样子, 第一对数组就变得有序了  
接着就是要把 第三个数组(橙色) 和 第四个数组 (稍微淡那么一点点的橙色) 合并  
这个过程也是差不多的, 最后合并完成是这个样子  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_merge5_copy.png?raw=true)  
接下来, 就要把 <span style="color: #FF0000;">红色数组</span> 和 <span style="color: #FF8000">橙色数组</span> 合并  
首先, 将 <span style="color: #FF0000;">红色数组</span> 的第一个元素 3 和 <span style="color: #FF8000">橙色数组</span> 的第一个元素 5 比较  
3 \< 5, 则将 3 移动到新数组中  
接着, 将 <span style="color: #FF0000;">红色数组</span> 的第一个元素(事实上是第二个元素, 但是因为第一个元素被删除了) 44 和 <span style="color: #FF8000">橙色数组</span> 的第一个元素 5 比较  
44 \> 5, 则将 5 移动到新数组中  
这么循环下去, 这两个数组最终会被合并成这样子  
![](https://github.com/HoshinoTented/hoshinotented.github.io/blob/master/resources/merging_sort/sorting_merge8_copy.png?raw=true)  
再这么继续下去, 所有的数组都会被合并成一个有序的数组  
这就是 **归并排序**  

# 实现代码 (Haskell)  
星野不太喜欢 C 实现的代码, 相比Haskell的太绕了  
**~~Haskell赛高~~**  
那么, 就上代码吧!  
```Haskell
module LuoGu.MergeSort where

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
    x < y then x : merge xs (y:ys)      -- 如果 第一个数组的首元素 小于 第二个数组的首元素, 则 排序剩下的两个数组, 并将 第一个数组的首
素添加在排序后的数组之前
    else y : merge (x:xs) ys            -- 这里跟上面差不多啦
```
虽然写出来跟上面演示的可能有点不大一样, 但是核心是一样的, 都是把 **两个有序的数组合并成一个有序的数组**