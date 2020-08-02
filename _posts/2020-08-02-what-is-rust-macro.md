---
layout: post
title: 什么是 Rust 宏？ —— 浅谈 Rust 宏
post: YES!
---

## 宏

> 计算机科学里的宏是一种抽象（Abstraction），它根据一系列预定义的规则替换一定的文本模式。解释器或编译器在遇到宏时会自动进行这一模式替换。对于编译语言，宏展开在编译时发生，进行宏展开的工具常被称为宏展开器。—— Wikipedia

C 所使用的 CPP（**C** **P**re**P**rocessor，即 C 预处理器）实现的宏是简单的文字查找与替换，不过它能接收一些编译时的参数来 “动态地” 替换代码。

## Rust 宏

而 Rust 的宏不是简单的文字查找与替换，Rust 宏基于 **语法树** 进行操作，比如：Rust 宏的参数可以指定类型（如 字面量 literal，表达式 expr 等等）

除了 类函数 的宏，Rust 还提供了另外两种宏：

* derive 宏：通过 `#[derive(...)]` 为 struct、trait 或 union 生成代码，如 `#[derive(Clone)]`
* attribute 宏：通过 `#[...]` 为 struct、trait、函数、字段、等等 生成代码，如 `#[serde(rename_all = "lowercase")]`

但以上两种宏都属于 **过程宏（Procedural Macros）**，本文不加讨论。

### 使用 Rust 宏

以最常见的 Rust 宏 `println!` 为例，你可以这样使用它：

```rust
let ar = "ar";
let gu = String::from("gu");
let s = 's';

println!("literal string, and here are the {}{}{}{}.", ar, gu, "ment", s);
```

我们来解析一下这个宏的调用：

* `println`：宏的名称，这里指 `println` 宏
* `!`：代表调用的是一个宏
* `(args...)`：传递给宏的参数

三者缺一不可

### 解析 Rust 宏的定义

以 `panic!` 宏为例：

[`std::panic`](https://doc.rust-lang.org/stable/std/macro.panic.html)

```rust
macro_rules! panic {
    () => ({ $crate::panic!("explicit panic") });
    ($msg:expr) => ({ $crate::rt::begin_panic($msg) });
    ($msg:expr,) => ({ $crate::panic!($msg) });
    ($fmt:expr, $($arg:tt)+) => ({
        $crate::rt::begin_panic_fmt(&$crate::format_args!($fmt, $($arg)+))
    });
}
```

* `macro_rules!`：代表以下为宏的定义
* `panic`：宏的名称

随后的大括号代表宏定义的内容。

宏定义的内容由 **规则（rule）** 组成，每一条规则都形如：

```rust
(Pattern) => { Exapnsion };
```

最后一条规则末尾的分号可以省略。

事实上，规则所用的括号可以是以下的任何一种：`()`，`[]`，`{}`。并且在调用的时候并不会对所用的括号进行检查，也就是说，你可以用 `foo! {}` 来调用定义为 `macro_rules! foo { () => {} }` 的规则。

当宏被调用时，会由上而下对每个规则进行匹配，如果某一条规则与输入 **完全** 匹配，则立刻进行该规则所对应的展开。考虑如下宏：

```rust
macro_rules! test {
    ( $( $e:expr )* ) => { println!("expr"); };
    ( $( $t:literal )* ) => { println!("literal"); };
}
```

当这个宏被调用时，即使传入的参数为字面量，都会立刻匹配到第一个规则，因为所有合法的字面量都是合法的表达式。

`panic!` 宏的第一条规则很好理解，当宏的参数为空时，调用 `panic!` 宏并传入一个 `"explicit panic"` 的字面量。这看起来是宏在调用自己，但实际上就是这样，这次调用会进入第二条规则。

第二条规则接收一个名为 `msg` 的 `expr` 类型的参数，参数的声明与使用都需要在前面加上 `$`。假如不加上，`msg:expr` 会被识别为一个字面量的 `tt`(token tree) 模式，你就只能通过以下方法调用它：

```rust
panic!(msg:expr);       // 实际上可以在 msg 的左右侧，expr 的左右侧加上空格，空格将会被忽略
```

第二条规则的展开内容的意思是：调用 `<panic! 所在的 crate>::rt::begin_panic` 函数，并传入 `msg` 作为参数。

第三条规则的模式和第二条规则大同小异，只是在最后面加了一个 `,`。而它的展开内容其实也只是调用了 `panic!`，最终会匹配第二条规则。

> 其实第 2、3 条规则的模式可以合并为 `$msg:expr $(,)?`，但作者猜测是因为早期 Rust 不支持 `?`，所以才分为了两条规则

最后是第四条规则，它接收两个参数：名为 `fmt` 的 `expr` 类型参数，和名为 `arg` 的 `tt` 类型的 **重复** 参数（即 可变数量 参数）。

### Rust 宏的参数类型

Rust 宏的参数目前有如下类型：

* `item`：[_Item_]，如函数定义，常量声明 等
* `block`：[_BlockExpression_]，如`{ ... }`
* `stmt`：[_Statement_]，如 `let` 表达式（传入为 stmt 类型的参数时不需要末尾的分号，但需要分号的 item 语句除外）
* `pat`：[_Pattern_]，模式匹配中的模式，如 `Some(a)`
* `expr`：[_Expression_]，表达式，如 `Vec::new()`
* `ty`：[_Type_]，类型，如 `i32`
* `ident`：[IDENTIFIER_OR_KEYWORD]，标识符或关键字，如 `i` 或 `self`
* `path`：[_TypePath_]，类型路径，如 `std::result::Result`
* `tt`：[_TokenTree_]，Token 树，被匹配的定界符 `(`、`[]` 或 `{}` 中的单个或多个 [token]
* `meta`：[_Attr_]，形如 `#[...]` 的属性中的内容
* `lifetime`：[LIFETIME_TOKEN]，生命周期 Token，如 `'static`
* `vis`：[_Visibility_]，可能为空的可见性限定符，如 `pub`
* `literal`：匹配 -? [_LiteralExpression_]

其中，`tt` 类型可以被视为 Rust 宏的 Any。

宏还对各种类型的参数捕获之后所允许的内容添加了限制，以避免语义冲突：

* `item`：任何标记
* `block`：任何标记
* `stmt`：`=>`、`;`、`,`
* `pat`：`=>`、`,`、`=`、`|`、`if` 或 `in`
* `expr`：`=>`、`;`、`,`
* `ty`：`{`、`[`、`=>`、`,`、`>`、`=`、`:`、`;`、`|`、`as` 或 `where`
* `ident`：任何标记
* `path`：`{`、`[`、`=>`、`,`、`>`、`=`、`:`、`;`、`|`、`as` 或 `where`
* `meta`：任何标记
* `tt`：任何标记

### Rust 宏的重复

宏中定义重复的语法如下：

```rust
$ ( ... ) sep rep
```

其中：

* `( ... )` 为要重复匹配的模式
* `sep` 为可选的分隔符，常见的有 `,` 和 `;`
* `rep` 为必选的重复标记，目前 Rust 支持三种重复标记：`*`（零次或多次）、`+`（一次或多次）、`?`（零次或一次）

举个例子：

* `$( $i:expr ),*` 可以匹配 “` `”、“`1, 2, 3`”，但不能匹配 “`1, 2, 3,`”
* `$( $i:expr, )*` 可以匹配 “` `”、“`1, 2, 3,`”，但不能匹配 “`1, 2, 3`”
* `$( $i:expr ),* $(,)?` 两者都能匹配

可以通过 `$( $i )*` 的语法来使用重复。

其实，还可以将两个重复参数作为一个整体，再进行重复，如：

```rust
macro_rules! zip {
    ( $( $i:expr ),* ; $( $j:expr ),* ) => {
        [ $( ($i, $j) )* ]
    };
}
```

不过塞到同一个整体进行重复的重复参数必须重复相同次数，否则编译器会 panic。

## 热身

有了上面的知识，我们就能写一个比较简单的 Vec 构造宏了：

需求：传入零个或多个相同类型的值，构造一个包含这些值（按照顺序）Vec。

```rust
macro_rules! build_vec {
    (
        $( $i:expr ),*                          // 重复，以支持任意数量的参数
        $(,)?                                   // 可选的末尾逗号
    ) => {
        {                                       // 创建一个块，以支持多条语句
            let mut vec = Vec::new();           // 构造一个 Vec，必须为 mut，否则下文无法进行 push

            $(                                  // 重复，将每个 $i 推入 vec 中
                vec.push($i);
            )*

            vec                                 // 返回 vec
        }
    }
}
```

> 标准库有类似的 `vec!` 宏，但其内部实现不与演示中的代码相同，有兴趣的可以到 [这里](https://doc.rust-lang.org/stable/alloc/macro.vec.html) 查看源代码

## 你已经学会了 Rust 宏，快来试试看吧？

Haskell 的列表生成器是个非常不错的语法糖：

```haskell
[1, 3 .. 10]
-- output: [1,3,5,7,9]
[1, 3 ..]
-- output: [1,3,5..]
[x + y | x <- [1..5], y <- [1..5]]
-- output: [2,3,4,5,6,3,4,5,6,7,4,5,6,7,8,5,6,7,8,9,6,7,8,9,10]
```

其中，`[1, 3 .. 10]` 和 `[1, 3 ..]` 可以用 Rust 的 `Range` 和 `RangeFrom` 结构体加上 `step_by` 函数实现：

```rust
macro_rules! list {
    // mathces list!(first, second, ..)
    // 注意，second 后面的 逗号 必须保留，原因在上文已经提到过了
    ( $first:expr, $second:expr, ..) => {
        {
            let step = $second - $first;
            ($first..).step_by(step)
        }
    };

    // matches list!(first, second, .. end) and list!(first, second, .., end)
    ( $first:expr, $second:expr, .. $(,)? $end:expr ) => {
        {
            let step = $second - $first;
            ($first..$end + 1).step_by(step)
        }
    };
}
```

来实验一下：

```rust
fn main() {
    println!("{:?}", list!(1, 3, .. 11).collect::<Vec<u32>>());
    println!("{:?}", list!(1, 3, ..).take(10).collect::<Vec<u32>>());       // 由于 RangeFrom 是一个无限的迭代器，所以需要使用 take

    // outputs:
    // [1, 3, 5, 7, 9, 11]
    // [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
}
```

接下来是最后一个，也是最强大的 Haskell 语法糖：

对于任意的 `[ generator x0 x1 x2 ... | x0 <- xs0, x1 <- xs1, x2 <- xs2 ...]` 可以转化为：

```haskell
xs0 >>= \x0 ->
    xs1 >>= \x1 ->
        generator x0 x1 <$> xs2       -- 最后一个列表的迭代需要使用 map
```

相应的 Rust 代码是这样：

```rust
xs0.flat_map(move |x0| {    // 必须进行 move，否则在 generator 调用的时候会抱怨生命周期
    xs1.flat_map(move |x1| {
        xs2.map(move |x2| {
            generator(x0, x1, x2)
        })
    })
})
```

这段冗长的 Rust 代码和 Haskell 的语法糖相比差太多了，不过我们可以用宏来解决！

首先要解决的问题是，我们无法使用重复来实现嵌套的 `flat_map`。所以我们需要编写一个辅助用的宏：

```rust
macro_rules! __rec_iter {
    (
        $gen:expr ;                 // 生成器，expr 类型
        $id:ident <- $it:expr,      // 当前的迭代语句
        $( $rest:tt )* ) => {       // 剩余代码
        $it.flat_map(move |$id| __rec_iter!($gen ; $( $rest )* ) )      // 生成一层 flat_map，其内部交给下一层 __rec_iter 宏
    };

    ( 
        $gen:expr ;                     // 生成器
        $id:ident <- $it:expr ) => {    // 当前的迭代语句，对于整个宏来说也是最后一个
        $it.map(move |$id| $gen)        // 生成 map，其内部由为 gen
    };
}
```

这个宏实际上就已经能够完成 Haskell 列表生成器的工作了，但是为了隐藏内部细节，所以再为 list 内添加一个规则，让它调用这个 __rec_iter：

```rust
macro_rules! list {
    ( $gen:expr ; $( $id:ident <- $it:expr ),* ) => {
        __rec_iter!( $gen ; $( $id <- $it ),* )
    };

    /* other rules */
}
```

终于写完了，让我们来测试一下：

```rust
fn main() {
    let result = list!(x + y ; x <- 1..=5, y <- 1..=5);
    println!("{:?}", result.collect::<Vec<i32>>());

    // output
    // [2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8, 5, 6, 7, 8, 9, 6, 7, 8, 9, 10]
}
```

正确的输出！

## 卫生性

卫生性也是宏的一个重要特性。考虑以下代码：

```rust
macro_rules! using_x {
    ( $action:expr ) => {
        {
            let x = 1;
            $action
        }
    }
}

fn main() {
    let two = using_x!(x + 1);
}
```

进行编译，编译器会抱怨 x 不在当前作用域内：

```plain
using_x!(x + 1);
         ^ not found in this scope
```

试着手动展开宏：

```rust
fn main() {
    let two = {
        let x = 1;
        x + 1
    }
}
```

这 **看起来** 好像没什么问题？为什么编译器会抱怨呢？

这是因为，宏内的 `x` 和 `$action` 的 `x` 处于不同的，看不见的 **句法上下文** 中，略微修改一下展开后的代码，让这个句法上下文 “看得见”：

```rust
fn main() {
    let two = {
        let macro_x = 1;
        outer_x + 1
    }
}
```

调用宏所使用的 `x` 属于 `outer` 上下文，而宏内的 `x` 属于 `macro` 上下文，只有在标识符的明面名字和句法上下文 **都** 一致的情况下，这两个标识符才能被视为相同。

我们可以通过提供一个 `ident` 类型的参数让两个标识符的句法上下文相同：

```rust
macro_rules! using_x {
    ( $id:ident, $action:expr ) => {
        {
            let $id = 1;
            $action
        }
    }
}

fn main() {
    let two = using_x!(x, x + 1);
}
```

它通过了编译，很好！

## 参考

[宏 - 维基百科](https://zh.wikipedia.org/wiki/%E5%B7%A8%E9%9B%86)

[The Little Book of Rust Macros](https://github.com/DanielKeep/tlborm)([中文](https://github.com/DaseinPhaos/tlborm-chinese))

[Macros - TRPL](https://doc.rust-lang.org/book/ch19-06-macros.html)([中文](https://kaisery.github.io/trpl-zh-cn/ch19-06-macros.html))

[Macro By Example](https://doc.rust-lang.org/reference/macros-by-example.html)

[_Item_]: https://doc.rust-lang.org/reference/items.html
[_BlockExpression_]: https://doc.rust-lang.org/reference/expressions/block-expr.html
[_Statement_]: https://doc.rust-lang.org/reference/statements.html
[_Pattern_]: https://doc.rust-lang.org/reference/patterns.html
[_Expression_]: https://doc.rust-lang.org/reference/expressions.html
[_Type_]: https://doc.rust-lang.org/reference/types.html#type-expressions
[IDENTIFIER_OR_KEYWORD]: https://doc.rust-lang.org/reference/identifiers.html
[_TypePath_]: https://doc.rust-lang.org/reference/paths.html#paths-in-types
[_TokenTree_]: https://doc.rust-lang.org/reference/macros.html#macro-invocation
[token]: https://doc.rust-lang.org/reference/tokens.html
[_Attr_]: https://doc.rust-lang.org/reference/attributes.html
[LIFETIME_TOKEN]: https://doc.rust-lang.org/reference/tokens.html#lifetimes-and-loop-labels
[_Visibility_]: https://doc.rust-lang.org/reference/visibility-and-privacy.html
[_LiteralExpression_]: https://doc.rust-lang.org/reference/expressions/literal-expr.html