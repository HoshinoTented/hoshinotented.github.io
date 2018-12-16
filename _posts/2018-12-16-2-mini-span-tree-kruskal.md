---
layout: post
title: 星野野的最小生成树 (二)
post: YES!
---

参考: [可视化算法](https://visualgo.net/zh)

# 开头
居然没有咕咕咕  
看来本小姐还是很勤奋的嘛!  

# 正文
上。。上次不是写了个 `普里姆 (Prim)` 的最小生成树算法嘛。。。  
然。。然后一不小心就开了个坑  
不过, 本小姐现在可是已经填了这个坑了哦!  

那么, 在这一篇 ~~辣鸡~~ 博客里本小姐将会介绍 `最小生成树` 的第二种算法 `克鲁斯卡尔 (Kruskal)`  
~~这名字怎么这么难记。。~~  

同样, 先上图(数据来自《大话数据结构》) ~~(因为没有保存图的数据。。所以这一次的图可能会跟上次的图的顶点有点偏差, 不过数据是一模一样的!)~~  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/init.png?raw=true)  
上一个算法(`普里姆 Prim`) 是从顶点出发, 构建最小生成树的  
而这一次要介绍的 `克鲁斯卡尔 (Kruskal)` ~~这也太难记了~~ 则是从边出发的  
首先, 将所有的边排好序, 从最短边出发(`3-6 (7)`)  
判断这条边加入最小生成树后, 是否会成环  
很显然, 最小生成树里什么都没有, 那么就将 `3-6 (7)` 放进最小生成树里  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/0.png?raw=true)  
接下来到了第二小的边 `1-7 (8)`  
判断是否会成环, 然后根据结果选择是否添加...  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/1.png?raw=true)  
如此循环下去, 直到...  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/2.png?raw=true)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/3.png?raw=true)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/4.png?raw=true)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/5.png?raw=true)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/6.png?raw=true)  
到这里的时候, 第 n 小的边是 `4-5 (17)`  
但是检验是否成环的时候发现  
如果这条边加入了最小生成树, 那么将会有 `环` 出现  
因此忽略掉 (心疼 `4-5 (17)`)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/7.png?raw=true)  
同样, `8-1 (18)` 也是会成环, 忽略  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/8.png?raw=true)  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/9.png?raw=true)  
直到把所有边都遍历完毕后, 最终的生成树, 会是这个样子的  
![](https://github.com/HoshinoTented/Resources/blob/master/mst-kruskal/10.png?raw=true)  

怎么样, 在本小姐的悉心指导下, 是不是学的很轻松  
~~明明是 Kruskal 本来就很简单~~  

`克鲁斯卡尔 (Kruskal)` 算法 ~~真的想给它重新名一个名字。。~~  
思路上很简单, 也十分暴力  
**就是将所有边从小到大排序好  
并遍历它们, 如果这条边加入后不会成环, 那么就放心的加入到最小生成树  
如果加入后会成环, 就忽略它**  

示例代码(选自《大话数据结构》, 有少量删改):  
```cpp
// 通过一个顶点来找到包含这个顶点的 最小生成树 的父节点
auto find(const size_t *parent, size_t f) -> size_t {
	while (parent[f] > 0) f = parent[f];
	return f;
}

auto miniSpanTree(const std::vector<Edge> &edges) -> void {
	size_t parent[MAX];     // 用来判断是否成环, 在另一种角度上可以把这个看成是 最小生成树 
	std::memset(parent, 0, sizeof(size_t) * MAX);   // 初始化

	for (const auto &edge : edges) {    // 遍历所有边
		size_t n = find(parent, edge.begin);    // 找到包含 edge.begin 的父节点
		size_t m = find(parent, edge.end);      // 找到包含 edge.end 的父节点

		if (n != m) {       // 如果这两个父节点不相同, 那么可以通过这一条边将他们连起来
			std::cout << edge.to_string() << std::endl;     // 输出边
			parent[n] = m;      // 更新最小生成树 
		}

        // 如果父节点相同, 加入这条边后就会成环, 因此选择忽略
	}
}
```

事实上, 本小姐对判断是否成环的代码并不是很懂。。。  
这。。这是秘密哦, 不要跟其他人说。。  