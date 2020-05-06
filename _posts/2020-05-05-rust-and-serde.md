---
layout: post
title: 浅谈 Rust 中 serde 库的使用
post: YES!
---

serde 是一个基于 Rust 的序列化库，可以把 Rust 的数据类型序列化为 serde 的数据模型，或是将字符串反序列化为 serde 的数据模型，再转化为 Rust 的数据类型。

## Learn By Example

首先通过一个简单的 JSON （反）序列化例子来认识 serde：

`Cargo.toml`

```toml
[dependencies]
serde = { version = "*", features = ["derive"] }
serde_json = "*"
```

`main.rs`

```rust
use serde::{Serialize, Deserialize};        // 导入自动实现 序列化/反序列化 的宏

#[derive(Debug, Serialize, Deserialize)]    // 自动实现序列化和反序列化
pub enum Gender {                           // 自定义的性别枚举结构体
    #[serde(alias = "female")]              // 为枚举添加别名
    Female,

    #[serde(alias = "male")]
    Male,
}

#[derive(Debug, Serialize, Deserialize)]
struct Person {                             // 自定义的结构体
    name: String,
    age: i32,
    gender: Gender
}

fn main() {
    let json = r#"{ "name": "Hoshino", "age": 4, "gender": "female" }"#;        // 被反序列化的 JSON 字符串
    let hoshino: Person = serde_json::from_str(json).unwrap();                  // 使用 serde_json 库进行 JSON 的反序列化

    println!("{:?}", hoshino);
}
```

大多数情况下，都可以用 derive 宏来自动实现结构体的序列化和反序列化，但是如果情况更复杂呢？比如要求性别不止有男性和女性：

```rust
pub enum Gender {
    Female,
    Male,
    Neither,
    Both,
    Trans { from: Gender, to: Gender }
}
```

这个时候，可以通过改良结构体的设计，但是这样可能会套很多层，不太方便，那么可以 **自定义序列化实现**。

## Custom Serialize

首先是自定义序列化：

```rust
#[derive(Debug)]
pub enum Gender {
    Female,                                         // 雌性
    Male,                                           // 雄性
    Neither,                                        // 无性
    Both,                                           // 中性
    Trans { from: Box<Gender>, to: Box<Gender> }    // 跨性别
}

impl Serialize for Gender {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
        S: Serializer {
        let str = match self {              // 根据枚举值来决定字符串内容
            Gender::Female => "female",
            Gender::Male => "male",
            Gender::Neither => "none",
            Gender::Both => "both",
            Gender::Trans { from, to } => {
                let pair = (from.as_ref(), to.as_ref());

                match pair {
                    (Gender::Female, Gender::Male) => "ftm",
                    (Gender::Male, Gender::Female) => "mtf",

                    _ => Err(<S as Serializer>::Error::custom("Trans variant only supports mtf and ftm"))?  // 为了简单这里只支持双性的跨性别
                }
            }
        };

        serializer.serialize_str(str)       // 序列化一个字符串
    }
}

#[derive(Debug, Serialize)]     // 注意删去 Deserialize，因为我们还没为 Gender 提供反序列化
struct Person {
    name: String,
    age: i32,
    gender: Gender
}

fn main() {
    let hoshino = Person {
        name: String::from("hoshino"),
        age: 4,
        gender: Gender::Trans { from: Gender::Male.into(), to: Gender::Female.into() }
    };

    let json = serde_json::to_string(&hoshino).unwrap();        // 使用 serde_json 库来进行 JSON 的序列化
    println!("{}", json);
}
```

输出：

```plain
{"name":"hoshino","age":4,"gender":"mtf"}
```

## Custom Deserialize

自定义的反序列化有些麻烦，你需要先实现一个相应的 `Visitor`，再为 `Deserializer` 提供 `Visitor` 实现：

```rust
impl <'de> Deserialize<'de> for Gender {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        struct GenderVisitor;           // Gender 的 Visitor，用来反序列化

        impl <'de> Visitor<'de> for GenderVisitor {
            type Value = Gender;            // Visitor 的类型参数，这里我们需要反序列化的最终目标是 Gender

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {        // 必须重写的函数，用于为预期之外的类型提供错误信息
                formatter.write_str("expect 'female', 'male', 'none', 'both', 'mtf' and 'ftm'")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where      // 从字符串中反序列化
                E: DeError, {
                let result = match v {          // 普通的模式匹配
                    "female" => Gender::Female,
                    "male" => Gender::Male,
                    "none" => Gender::Neither,
                    "both" => Gender::Both,
                    "mtf" | "MtF" => Gender::Trans { from: Gender::Male.into(), to: Gender::Female.into() },
                    "ftm" | "FtM" => Gender::Trans { from: Gender::Female.into(), to: Gender::Male.into() },

                    _ => Err(E::custom(format!("unexpected: {}", v)))?  // 同上，为了简单起见不支持 mtf 和 ftm 之外的跨性别
                };

                Ok(result)
            }
        }

        deserializer.deserialize_str(GenderVisitor)         // 为 Deserializer 提供 Visitor
    }
}

fn main() {
    let json = r#"{ "name": "Hoshino", "age": 4, "gender": "mtf" }"#;
    let hoshino: Person = serde_json::from_str(json).unwrap();

    println!("{:?}", hoshino);
}
```

输出：

```plain
Person { name: "Hoshino", age: 4, gender: Trans { from: Male, to: Female } }
```

## (De)Serializing When (De)Serializing

如果要把生理性别和心理性别分开来的话：

```rust
#[serde(rename_all = "lowercase")]              // 使用 rename_all 将所有枚举值重命名为小写
#[derive(Debug, Deserialize, Serialize)]        // 这里使用 derive 简化代码，Sex 的序列化不是本节要讨论的内容
pub enum Sex {
    Female,
    Male,
}

#[derive(Debug)]
pub enum Gender {
    Common(Sex),            // 常见的心理性别
    Neither,                // 无性
    Both,                   // 双性（或者中性）
    Trans(Sex)              // 跨性别
}
```

实现 Gender 的序列化：

```rust
impl Serialize for Gender {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
        S: Serializer {
        let str = match self {
            Gender::Common(sex) => return sex.serialize(serializer),        // 将序列化器交给 sex 进行序列化（注意：serialize 会吃掉所有权）
            Gender::Neither => "none",
            Gender::Both => "both",
            Gender::Trans(Sex::Female) => "ftm",
            Gender::Trans(Sex::Male) => "mtf",
        };

        serializer.serialize_str(str)
}

impl <'de> Deserialize<'de> for Gender {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        struct GenderVisitor;

        impl <'de> Visitor<'de> for GenderVisitor {
            type Value = Gender;

            fn expecting<'a>(&self, formatter: &mut Formatter<'a>) -> std::fmt::Result {
                formatter.write_str("expect female, male, none, both, mtf and ftm")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where
                E: DeError, {
                // 这里需要显式标注，否则会出现编译错误
                let der: StringDeserializer<E> = v.to_string().into_deserializer();     // 将基础类型 str 转换为反序列化器
                let try_common = Sex::deserialize(der);     // 调用 Sex 的反序列化函数，返回 Result

                let result = match try_common {             // 对 Result 进行模式匹配
                    Ok(sex) => Gender::Common(sex),         // 反序列化成功，构造 Common
                    Err(_) => match v {                     // 反序列化失败，匹配其他值
                        "none" => Gender::Neither,
                        "both" => Gender::Both,
                        "mtf" => Gender::Trans(Sex::Male),
                        "ftm" => Gender::Trans(Sex::Female),

                        _ => Err(E::custom(format!("unexpected {}", v)))?,
                    }
                };

                Ok(result)
            }
        }

        deserializer.deserialize_str(GenderVisitor)
    }
}
```

## 最后

serde 不仅能（反）序列化为 JSON，还有 YAML，TOML 等多种语言，可以在 serde 的[官方文档](https://serde.rs)中找到它们。
