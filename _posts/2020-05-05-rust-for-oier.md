---
layout: post
title: Rust for OIer
post: YES!
---

Rust 是一门原生的命令式语言，但同样也有着函数式的特性，还有其独特的所有权系统，免去编写者手动处理内存。

## Hello world!

学习任何一门语言都可以从 Hello world 开始，让我们来看看 Rust 的 Hello world 是什么样的吧：

```rust
fn main() {
    println!("Hello, world!");
}
```

* `fn` 是函数声明的关键字。
* `main` 是函数名称，和 C 一样，Rust 同样把 main 函数作为程序的入口。
* 用 `{ ... }` 来代表函数体。
* `println!` 用 println **宏** 来输出字符串。
* `"Hello, world!"` 用双引号代表字符串，单引号代表字符。
* `;` 不作为返回值的表达式应用分号结尾

## 变量

用 `let` 声明一个**不变的**变量：

```rust
fn main() {
    let i = 1;          // Rust 可以自动推断类型
    // i = 2;           // 报错
}
```

`let` 后面接上 `mut` 代表变量是**可变的**：

```rust
fn main() {
    let mut i = 1;      // 可变的 i
    i = 2;              // 编译通过
}
```

## 函数

用 `fn` 声明函数：

```rust
fn foo() {
    // do something
}
```

参数格式为：`name: Type`

```rust
// i32 相当于 C 中的 int
fn foo(i: i32, j: i32) {
    // do something
}
```

返回值类型格式为：`-> Type`

```rust
fn foo(i: i32, j: i32) -> i32 {
    // do something
}
```

用 `return` 关键字显式返回值，或者以函数最后一行不加分号的表达式作为返回值：

```rust
fn foo(i: i32, j: i32) -> i32 {
    i + j
}
```

## 输入输出

使用 `println!` **宏** 输出字符串：

```rust
fn main() {
    println!("Hello, world!");
}
```

使用 `{}` 作为占位符进行格式化：

```rust
fn main() {
    println!("{}, {}!", "Hello", "world");
}
```

使用 `std::io::stdin` 函数读入一行：

```rust
use std::io::stdin;

fn main() {
    let mut line = String::new();           // 用 String::new 构造一个空字符串

    stdin()
        .read_line(&mut line)               // &mut line 是获取 line 的一个可变引用
        .expect("input error");             // 处理读取输入时可能导致的错误

    println!("{}", line);                   // 格式化字符串必须为原生字符串
}
```

用 `trim` 和 `split` 来分割字符串：

```rust
fn main() {
    let mut line = String::new();

    /* ... */

    let mut iter = line.trim()         // trim 是为了删除后尾的空格和换行符
                       .split(" ");    // 用空格分割字符串，返回的是一个 Split 迭代器
}
```

用 `next` 来读取迭代器内容，用 `parse` 来转换为数字类型：

```rust
fn main() {
    let mut iter = /* ... */
    let a = iter
            .next().unwrap()            // 读取下一个字符串，可能为空，unwrap 函数是：当为空时抛出错误，Rust 中将抛出错误称为 panic
            .parse::<i32>().unwrap();   // 解析为 i32 类型，同样用 unwrap 函数，因为解析可能出错
    let b: i32 = iter.next().unwrap().parse().unwrap();
}
```

## 条件控制

if 语句：

```rust
fn main() {
    let i = 1;
    if i == 2 {       // 不需要圆括号，但是不能省略大括号
        // TODO
    }
}
```

if-else 语句：

```rust
fn main() {
    let i = 1;

    if i == 2 {
        // TODO
    } else {
        // TODO
    }
}
```

if else-if else 语句

```rust
fn main() {
    let i = 1;

    if i == 2{

    } else if i == 3 {

    } else {

    }
}
```

`if-else` 和 `if else-if else` 语句可以具有值：

```rust
fn main() {
    let i = 1;
    let j = if i == 1 {
        2                   // 这里没有分号
    } else {
        3
    };                      // 这里有分号
}
```

用 `loop` 代表无止境的循环：

```rust
fn main() {
    loop {
        println!("LOOOOOOOOOOOOOOOOOOOOOOOOOOOOOP");
    }
}
```

用 `while` 来进行有条件的循环：

```rust
fn main() {
    let mut sum = 0;
    let mut i = 1;

    while i <= 100 {
        sum = sum + i;
        i = i + 1;
    }

    println!("{}", sum);
}
```

用 `for` 来对迭代器进行迭代：

```rust
fn main() {
    let vec = vec![1, 2, 3];        // 参见下文，这里构造了一个可变长度数组

    for i in vec.iter() {           // 参见下文
        println!("{}", i);
    }
}
```

## 容器

使用原生数组：

```rust
fn main() {
    let arr = [0; 10];      // 长度为 10 的数组，默认值为 0，这个数组的类型是 [i32; 10]
    println!("{}", arr[0]); // 下标从 0 开始
}
```

使用 `Vec` 可变长度数组：

```rust
fn main() {
    let mut vec = Vec::new();   // 使用 Vec::new 构造一个 0 长度的 Vec

    vec.push(1);                // 添加元素，并通过参数类型推断 Vec 内元素类型
                                // 此时 vec 类型为：Vec<i32>

    vec.push(2);

    println!("{}", vec[0]);
}
```

使用 `get` 函数安全地获取元素：

```rust
fn main() {
    let mut vec = vec![1, 2, 3];        // vec 宏，创建一个带有 1、2、3 元素的 Vec

    match vec.get(0) {                  // 获取下标为 0 的元素，返回值是 Option，这里对 Option 进行了模式匹配
        None => {                       // None
            println!("No such element");
        }

        Some(v) => {
            println!("{}", v);
        }
    }
}
```

什么是模式匹配？你可以当做把它们按照构造的样子拆开来，比如：

```rust
fn main() {
    let opt = Some(1);      // 用 Some(1) 构造

    match opt {
        None => {}      // 如果 opt 是按照 None 模式构造的，则运行代码块
        Some(v) => {}   // 如果 opt 是按照 Some(v) 模式构造的，则运行代码块
    }
}
```

有的时候你可能只需要一个模式，那么你可以用 `if-let`：

```rust
fn main() {
    let vec = vec![1, 2, 3];

    if let Some(i) = vec.get(1) {
        println!("{}", i);
    }
}
```

类似的，也有 `while-let`：

```rust
fn main() {
    let vec = vec![1, 2, 3];
    let mut iter = vec.iter();              // 使用 iter 函数获得 vec 的迭代器

    while let Some(elem) = iter.next() {    // 使用 next 获得下一个元素，类型是 Option<&i32>，因为可能没有下一个元素
        println!("{}", elem);
    }
}
```

## 自定义结构体

用 `struct` 关键字定义一个结构体：

```rust
struct A {
    i: i32,
    s: String
}

fn main() {
    let a = A {                 // 构造结构体
        i: 123,
        s: "456".to_string()    // "456" 是一个 &str，不是 String，使用 .to_string 转化为 String
    };
}
```

使用 `enum` 定义一个枚举结构体：

```rust
enum B {
    B0(i32),
    B1 { i: i32, s: String }
}

fn main() {
    let b0 = B::B0(1);
    let b1 = B::B1 { i: 2, s: String::from("3") };       // 也可以用 String::from 函数将 &str 转化为 String
}
```

可以对结构体和枚举结构体进行模式匹配：

```rust
enum B {
    B0(i32),
    B1 { i: i32, s: String }
}

fn main() {
    let b0 = B::B0(1);
    let b1 = B::B1 { i: 2, s: String::from("3") };

    match b0 {
        B::B0(i) => { /* ... */ }       // 匹配 B0
        B::B1 { i /* 使用原本的名称 */, s: string /* 对 s 进行重命名 */ } => {}    // 匹配 B1
    }
}
```

## 浅谈所有权

Rust 吸引人的地方之一是它的所有权系统，所有权系统可以巧妙地自动释放内存，那什么是所有权呢？看看这段代码：

```rust
fn main() {
    let s0 = String::new();
    let s1 = s0;

    println!("{}", s0);
}
```

猜猜看，能不能编译成功呢？

```plain
error[E0382]: borrow of moved value: `s0`
 --> src\bin\test.rs:5:20
  |
2 |     let s0 = String::new();
  |         -- move occurs because `s0` has type `std::string::String`, which does not implement the `Copy` trait
3 |     let s1 = s0;
  |              -- value moved here
4 | 
5 |     println!("{}", s0);
  |                    ^^ value borrowed here after move
```

并不能，因为 `let s1 = s0` 代表着，把 s0 对 String 的所有权交给了 s1。因此再想进行 println! 操作的话，s0 一无所有，没有什么可以输出的了。

但你可以把 s0 的 String 借给 s1：

```rust
fn main() {
    let s0 = String::from("123");
    let s1 = &s0;               // 借给 s1

    println!("{}", s0);
    println!("{}", s1);
}
```

模式匹配也会进行所有权转移：

```rust
fn main() {
    let opt = Some(String::from("123"));

    match opt {
        None => {}
        Some(v) => {}
    }

    println!("{:?}", opt);       // 编译出错
}
```

有些读者可能尝试了这样的代码：

```rust
fn main() {
    let opt = Some(1);

    match opt {
        None => {}
        Some(v) => {}
    }

    println!("{:?}", opt);
}
```

顺利通过了编译，这是为什么呢？

因为 i32 类型实现了 Copy trait，但 trait 不在本文的讨论范围内，这里简单讲解一下：
实现了 Copy trait 的类型，会在转移所有权的地方进行自动复制，比如：

```rust
fn main() {
    let a = 1;
    let b = a;      // i32 实现了 Copy trait，这里并没有进行所有权转移，而是复制

    println!("{}", a);
    println!("{}", b);
}
```

但换做 String 类型则会出错，因为 String 类型没有实现 Copy trait。

## 结尾

Rust 是一门很有趣的语言。
嗯，就这样。
