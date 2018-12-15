---
layout: post
title: 星野野的最小生成树 (一)
post: YES!
---

参考: [可视化算法](https://visualgo.net/zh)

# 开头
`(一)` 是什么意思。。  
本小姐还不想开坑  

# 正文
本小姐在自己的反思 ~~(他人的督促)~~ 下  
选择学习了 `最小生成树`  

最小生成树似乎有两种算法  
本小姐在这篇博客讨论的将会是 `普里姆 (Prime) 算法` ~~这就是为什么标题会有 `(一)` 这种东西的原因了~~  

在介绍普里姆算法的思路之前, 本小姐将会展示关于这个算法的图片(请无视鼠标)  
首先, 本小姐有这样的一个图(数据来自《大话数据结构》):   
![](https://github.com/HoshinoTented/Resources/blob/master/mst-prime/init.png?raw=true)  
以 `4` 为顶点, 开始我们的最小生成树  
与 `4` 相关(有边)的节点有, `5` 和 `3`, 并将它们的边记录下来  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-prime/0.png?raw=true)  
寻找记录的边的最小值, 找到了 `11(4-5)`  
发现 `5` 还没有被加入最小生成树  
则现将 `5` 加入最小生成树, 并以 `5` 为顶点, 寻找相关的节点, 并将它们的边记录下来  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-prime/1.png?raw=true)  
这样执行下去...   
直到...  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-prime/5.png?raw=true)  
我们通过 `6` 找到了通往 `1`, `7` 和 `3` 的道路  
接下来重新寻找已经记录的边的最小值, 找到了 `17(6-3)`  
但是在确认 `17(6-3)` 的时候发现, `3` 已经被加入最小生成树了, 那么忽略  
接下来, 找到了 `18(5-0)`  
确认的时候也发现 `0` 已经加入最小生成树了, 同样忽略  
接下来找到了 `19(6-7)`, `7` 还没有被加入最小生成树, 那么继续重复以上的动作 ...  
最后的最小生成树将会是这个样子的:  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-prime/9.png?raw=true)  

会发现, 普里姆算法其实就是 **通过一个顶点寻找相邻的顶点, 并记录通往顶点的路径, 再找到已经记录的最小的路径, 走到另一个顶点**, 再通过这个顶点寻找相邻的顶点, 找到已经记录的最小的路径...  

接下来就是代码了(选自《大话数据结构》, 有删改)
```cpp
auto miniSpanTree(Graph g) -> void {
	Edge edges[MAX];		// 暂存的边集合

        // 以 0 为起点
	for (size_t i = 1; i < MAX; ++ i) {
		edges[i] = {0, i, g[0][i]};		// 向 边集合 存入 与 0 相关的边
	}

	for (int i = 1; i < MAX; ++ i) {
		int min = INF;	// 最小边的边长, 这里 INF 为 65535
		size_t k = 0;		// 最小边的顶点

		// 找到 edges 暂存的最小边
		for (size_t j = 1; j < MAX; ++ j) {

			// 如果 当前边的权 不为 0 (没有被加入最小生成树) 且 当前边长度 小于 min ( 当前记录的最小长度
			if (edges[j].weight != 0 && edges[j].weight < min) {

				// 更新最小长度 为 edges[j].weight
				min = edges[j].weight;

				// 更新最小长度边的一个顶点为 j
				k = j;
			}
		}

		std::cout << "(" << edges[k].begin << ", " << k << ")" << std::endl;		//输出最小边

		edges[k].weight = 0;		//将最小边设为 0 (加入最小生成树

		// 寻找与当前顶点相连的所有边
		for (size_t j = 1; j < MAX; ++ j) {

			// 如果 j 顶点没有列入最小生成树 且 当前顶点到目标顶点的边长小于记录的边长
			if (edges[j].weight != 0 && g[k][j] < edges[j].weight) {
				edges[j] = {k, j, g[k][j]};
			}
		}
	}
}
```

# 最后
听说 Prime 算法不太好用, 第二个算法才会比较实用的时候  
本小姐就 `kowasareta`  
没。。没事, 不就是学习一个新的算法嘛, 怎么可能会难倒本小姐。。。  
