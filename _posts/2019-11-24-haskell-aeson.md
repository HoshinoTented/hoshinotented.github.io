---
layout: post
title: Haskell JSON 解析库：Aeson
post: YES!
---

Aeson 是 Haskell 的 JSON 解析库，接下来让我们看看它都能做些什么吧！

## 从 JSON 到 Haskell

给出以下 JSON：

```json
{ "name" : "Joe", "age" : 12 }
```

如果我们需要将其从 JSON 解析成 Haskell 数据：

```haskell
data Person = Person {
    name :: String,
    age :: Int
} deriving (Show)
```

可以使用 `FromJSON` 类型类：

```haskell
class FromJSON a where
  parseJSON :: Value -> Parser a
```

`FromJSON` 类型类需要实现 `parseJSON` 函数：提供一个 `Value` （JSON 数据），返回一个 `Parser` Monad，一个简单的实现方法是这样的：

```haskell
instance FromJSON Person where
    parseJSON = withObject "Person" $ \obj -> Person
        <$> obj .: "name"
        <*> obj .: "age"
```

然后，我们就能使用 `decode` 或者 `fromJSON` 函数，将 JSON 字符串或数据转换成 Haskell 数据：

```haskell
decode :: FromJSON a => ByteString -> Maybe a
fromJSON :: FromJSON a => Value -> Result a

> decode "{ \"name\" : \"Joe\", \"age\" : 12 }" :: Maybe Person
Just (Person {name = "Joe", age = 12})
```

## 从 Haskell 到 JSON

时常也需要将 Haskell 数据转化为 JSON，这个时候可以使用 `ToJSON` 类型类：

```haskell
class ToJSON a where
  toJSON :: a -> Value
```

`ToJSON` 类型类需要实现 `toJSON` 函数：提供一个 `a` （Haskell 数据），返回一个 `Value` （JSON 数据）。
假如我们要将 `Person` 数据类型转化为 JSON，可以这样实现：

```haskell
instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age ]
```

然后就可以用 `encode` 或者 `toJSON` 函数，将 Haskell 数据转化为 JSON 字符串或数据：

```haskell
encode :: ToJSON a => a -> ByteString
toJSON :: ToJSON a => a -> Value

> encode (Person "Joe" 12)
"{\"age\":12,\"name\":\"Joe\"}"
```

## 解放双手

aeson 通过 `Generic` 类型类实现了自动生成 `FromJSON` 和 `ToJSON` 的功能：

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

data Person = Person {
    name :: String,
    age :: Int
} deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
```

这时候就不需要我们手动实现 `FromJSON` 和 `ToJSON` 了，因为 aeson 已经通过 `Generic` 类型类自动实现了。

## 深入敌后

来看看在上面用到的 `withObject` 函数：

```haskell
withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _ f (Object obj) = f obj
withObject name _ v = prependContext name (typeMismatch "Object" v)
```

`withObject` 接收一个 名称（通过实现就能看出对正常解析没有影响，仅作用于报错），一个 解析函数，一个 JSON 值（通过实现也可以看出，如果这个 JSON 值不是 Object 就会抛出异常），最后返回解析函数返回的 `Parser` Monad。

```haskell
(.:) :: (FromJSON a) => Object -> Text -> Parser a
```

`(.:)` 接收一个 `Object`（JSON 对象） 和一个 `Text`（字段名称），最后返回一个 `Parser` Monad。

当然还有 `Maybe` 版本的 `(.:)`：

```haskell
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
```

## 打开神奇的箱子

会发现以上函数大多都是围绕着 `Parser` 这个神奇的东西，接下来就讨论如何将 `Parser` 里的东西取出来：

```haskell
import Data.Aeson.Types

parse :: (a -> Parser b) -> a -> Result b
```

`parse` 接收一个 `(a -> Parser b)` 的转换函数，和一个参数 `a` （即为前面函数的参数值），最后返回一个解析结果。