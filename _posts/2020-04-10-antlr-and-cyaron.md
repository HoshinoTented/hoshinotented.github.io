---
layout: post
title: ANTLR4 与 CYaRon! 语言
post: YES!
---

## ANTLR4

[ANTLR4](https://github.com/antlr/antlr4) 是一款语法生成器，所谓的语法生成器就是通过编写的某种特定的 语法，来生成解析这种语法的解析器。
而这种解析器，则称为 **Parser**。

## 解析语法

解析语法有两个重要的概念：词法解析器（Lexer）和语法解析器（Parser）

词法解析器一般是将源代码（例如字符串）转化为 Token 序列，这是为了方便 Parser 解析：
例如字符串 `foo(1, "2")` 转换成 Token 序列 `[Symbol(foo), LB, Number(1), COMMA, String("2"), RB]`，
将难以解析的字符按照某种特定的语法去处理成单独的 Token。

而语法解析器就是将 Token 转化为抽象语法树（**A**bstract **S**yntax **T**ree），或许某些人有见过表达式树：

`1 + 2`

与

```plain
+
|-1
|-2
```

表达式树也是抽象语法树，比如上面的 `foo(1, "2")` 可以表达为：

```plain
FunCall(foo)
|-<arguments>
  |-Number(1)
  |-String("2")
```

复杂一点的，比如 C 语言的 `if (i == 1) { printf("%d", i); }`：

```plain
if
|-<condition>
  |-Eq
    |-Symbol(i)
    |-Number(1)
|-<statement>
  |-FunCall(printf)
    |-<arguments>
      |-String("%d")
      |-Symbol(i)
```

最终，通过编写这些 AST 的求值代码，就得到一个解释器了。

## ANTLR4 语法

ANTLR4 用大写开头的名称表示 Token，例如：

```antlr
Number: [0-9]+;
Add: '+';
Mul: '*';
```

`Number` 是 Token 的名称，而冒号到分号的内容，是 `Number` 所解析的字符串内容。
比如 `[0-9]` 代表 `'0'` 到 `'9'` 之间的所有字符，而 `+` 代表一个或多个。
这和正则表达式的语法很像，`*` 代表零个或多个，而 `?` 代表零个或一个。

用小写开头的名称表示规则，例如：

```antlr
exp: add | mul | number;
add: exp Add exp;
mul: exp Mul exp;
number: Number;
```

`exp` 是 规则 的名称，而 `|` 则代表 `或`，比如 `exp` 规则解析 `add` 规则或 `mul` 规则，或者是 `number` 规则。
规则可以使用 Token，反之不可。

### 歧义

但仔细看看，`1+2*3` 有不同的解析方法：

```plain
  exp
= exp Add exp
= 1 + exp
= 1 + exp Mul exp
= 1 + 2 * 3
```

或者是

```plain
  exp
= exp Mul exp
= exp * 3
= exp Add exp * 3
= 1 + 2 * 3
```

这种语法规则是有歧义的，而且从这歧义中可以看出，这份语法规则并没有体现出加法和乘法的优先级关系。

对于二元运算 `+` 和 `*`，如果 `*` 比 `+` 优先级高，则应该写成：

```antlr
exp: add;
add: add + mul | mul;
mul: mul * number | number;
number: Number;
```

> 题外话：如果 `+` 是左结合的，那么应该写成 `A: A + B | B;`，如果是右结合的，则应该写成 `A: B + A | B;`，如果无结合性，则应该写成 `A: B + B | B;`

### 左递归

再仔细看看：

```antlr
exp: add;
add: add + mul | mul;
```

`add` 会直接解析 `add` 规则，再去解析后续的规则。
这种递归是无止境的，让解析器进入无穷递归，最终耗尽物理资源。
这种递归被称为左递归，某些词法生成器会提供针对左递归的语法，ANTLR4 也有，但还是需要学习一下如何消除左递归的知识。

以二元运算加法为例，消除左递归一般是把 `1+2` 的 `+2` 部分看做一个整体：

```antlr
exp: add;
add: mul add_;
add_: (Add add add_)?;
mul: number mul_;
mul_: (Mul number mul_)?;
number: Number;
```

在[维基百科](https://zh.wikipedia.org/zh-cn/%E5%B7%A6%E9%81%9E%E6%AD%B8)有更详细的说明。

最终的语法代码应该是这样的：

```antlr
Number: [0-9]+;
Add: '+';
Mul: '*';

exp: add;
add: mul add_;
add_: (Add mul add_)?;
mul: number mul_;
mul_: (Mul number mul_)?;
number: Number;
```

## CYaRon! 语言

[CYaRon! ~~（以下简称瞎龙）~~ 语言](https://www.luogu.com.cn/problem/P3695) 是一门语法比较简单，功能不完善的语言，接下来我就试着用 ANTLR4 来编写瞎龙语言的语法解析器。

首先是 Token 部分，先把 **所有** 字面量都表示出来：

```antlr
Vars: 'vars';
For: 'hor';
If: 'ihu';
While: 'while';
BlockStart: '{';
BlockEnd: '}';
Int: 'int';
Array: 'array';
ArrStart: '[';
ArrEnd: ']';
To: '..';
Comma: ',';
Colon: ':';
Plus: '+';
Minus: '-';
LT: 'lt';
GT: 'gt';
LE: 'le';
GE: 'ge';
EQ: 'eq';
NEQ: 'neq';

Digit: [0-9]+;
Symbol: [a-zA-Z]+;
Comment: '#' ~[\r\n]* '\r'? '\n' -> skip;
WS: [\n\t\r ]+ -> skip;
```

比较特别的是最后两个，`-> skip` 代表这两个 Token 会被 Parser 忽略。

接下来是规则部分，按照题目的代码示例，先实现定义变量部分：

```antlr
definitionBlock:
    BlockStart Vars
    definitionLine*
    BlockEnd
```

接下来是变量定义的 **行** 部分：

```antlr
definitionLine:
    WS* Symbol Colon type
```

好像因为 `*` 是连续的 规则，中间不能夹杂其他东西，所以要手动加上 `WS*`。不过行尾没加也不会报错，可能是某些奇妙的魔法……

类型：

```antlr
typeInt: Int;
typeArray: Array ArrStart type Comma Digit To Digit ArrEnd;
type: typeInt | typeArray;
```

函数调用：

```antlr
opt: Plus | Minus;                                              // 二元运算符
indexing: ArrStart rightValue ArrEnd;                           // 数组索引规则 [1]

funCall: Colon Symbol parameter?;                               // 函数调用规则 :yosoro 1, a
parameter: rightValue | rightValue (Comma rightValue)*;         // 参数规则 1, a
leftValue: Symbol | leftValue indexing;                         // 左值规则 1, a, arr[1][2]
formulaValue : leftValue | Digit | funCall;                     // 算术值规则，仅用于二元运算解析用
rightValue: formulaValue | formula;                             // 右值规则，包含函数调用
formula: formulaValue formula_;                                 // 二元运算规则
formula_: (opt formula formula_)?;
```

表达式块：

```antlr
statementLine: WS* (funCall | definitionBlock | ifBlock | whileBlock | forBlock);       // 单行代码
statement: statementLine*;                                                              // 多行代码
```

if、for、while 块：

```antlr
condOpt: LT | GT | LE | GE | EQ | NEQ;                      // 条件运算符
cond: condOpt Comma rightValue Comma rightValue;            // 条件规则
forCond: rightValue Comma rightValue Comma rightValue;      // for 块条件规则

// if 块
ifBlock:
    BlockStart If cond
    statement
    BlockEnd;

// while 块
whileBlock:
    BlockStart While cond
    statement
    BlockEnd;

// for 块
forBlock:
    BlockStart For forCond
    statement
    BlockEnd;
```

这些就是瞎龙语言的全部语法代码了，statement 规则也可以作为整个文件的根规则进行解析。
