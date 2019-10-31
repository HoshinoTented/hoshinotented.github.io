---
layout: post
title: 好耶！是 State Monad！
post: YES!
---

State Monad 维护了一个状态，可以用来模拟变量，这在纯函数式的编程当中是非常便利的。

## Stack?

假如要实现一个栈，需要怎么做呢？

```haskell
push :: a -> [a] -> [a]
push = (:)

pop :: [a] -> (a, [a])
pop [] = error "qaq"
pop (x:xs) = (x, xs)
```

而在使用的时候……

```haskell
uncomfortableStack :: [Int] -> [Int]
uncomfortableStack xs = let
                          xs' = push xs 1
                          (x, xs'') = pop xs'
                          xs''' = push $ x + 1
                          (x', xs'''') = pop xs'''
                          xs''''' = push $ x' + 2 in
                            xs'''''
```

会发现花费了许多代码在维护 栈 上，接下来要介绍的 State Monad，就可以十分优雅地维护一个栈了。

## Let me see...

State Monad 用来维护一个 **状态（State）**。在编程中常常需要维护一个状态，比如一个变量。

接下来看看 State 的定义（在 mtl 库中不是这么定义的，接下来会解释其原因）：

```haskell
newtype State s a = State { runState :: s -> (a, s) } deriving (Functor)
```

State 包含了一个函数，接收一个状态（s），进行运算后返回一个结果（a）和一个新状态。
既然 State 是一个 Monad，那就再看看 State Monad 的实现。

```haskell
instance Monad (State s) where
    return a = State $ \s -> (a, s)
    (State m) >>= f = State $ \s ->
        let (v, s') = m s
            (State g) = f v in
            g s'

instance Applicative (State s) where
    pure = return
    (<*>) = ap
```

`return` 函数仅仅将一个值包装进 State Monad，而不对状态做任何修改，所以只需要 `\s -> (a, s)`。
而 `(>>=)` 需要把 State 中的 结果 取出来，所以需要有一个 来自未来 的状态 `s` 传入到状态中的 runState 拿到结果和一个新的状态。
接着用 f 应用到 得到的结果 上，就又得到了一个新的 State。
最后把得到的新 State 中的 runState 应用在得到的新状态上，得到属于 来自未来 的状态计算的结果。

## State!

回到一开始介绍的栈问题，要怎么用 State 来实现一个栈？

```haskell
push :: a -> State [a] ()
push i = State $ \xs -> ((), i:xs)
```

`push` 动作不会返回任何结果，所以是 `()` 值，而它会修改状态，即栈，所以是 `(i:xs)`。

```haskell
pop :: Stack [a] a
pop = State $ \xs -> case xs of
    [] -> error "qaq"
    (x:xs) -> (x, xs)
```

`pop` 动作会从栈中弹出一个值并返回，所以结果位置是 `x`，即栈顶，并且也会修改状态，因为从栈中弹出元素了，所以状态位置是 `xs`，即除栈顶以外的元素。

那要如何使用呢？

```haskell
wonderfulStack :: State [Int] ()
wonderfulStack = do
  push 1
  v <- pop
  push $ v + 1
  v' <- pop
  push $ v + 2
```

优雅了很多，不是吗？
但我们最后拿到的是一个 `State [a] ()`，而不是一个代表着栈的列表，所以需要用到 `runState` 或者 `evalState`：

```haskell
evalState :: State s a -> s -> a
-- evalState = (fst .) . runState       -- 请允许我失礼地 pointfree 一下
evalState state s = fst (runState state s)

stack :: [a]
stack = evalStack wonderfulStack []
```

`evalStack` 向 State 中传入了 来自未来 的状态，也是初始状态，进行了一系列的运算之后，拿到了最终的值。

## 不，这不够函数式！

对于 State，Haskell 提供了更高层次的抽象：MonadState

```haskell
class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  -- put = State . const . (,) ()
  put s = State $ \_ -> ((), s)
```

`get` 是获取当前状态，而 `push` 则是修改当前的状态，来看看要怎么用这两个函数：

```haskell
push' :: a -> State [a] ()
push' i = do
  xs <- get
  put (i:xs)

pop' :: State [a] a
pop' = do
  xs <- get

  case xs of
    [] -> error "qaq"
    (x:xs) -> put xs >> return x
```

看起来好像比之前的要长，但很明显更有可读性。

## 我的构造器呢？这么大的构造器呢？

`State` 被包含在 mtl 库中的 `Control.Monad.State` 包中。但读者在使用这个 `State` 的时候会发现：

```plain
> State $ \(x:xs) -> (x, xs)
<interactive>:22:1: error:
    • Data constructor not in scope: State :: ([a0] -> (a0, [a0])) -> t
    • Perhaps you meant one of these:
        ‘StateT’ (imported from Control.Monad.State),
        variable ‘state’ (imported from Control.Monad.State)
```

我们无法直接使用 `State` 这个构造器，而 GHCi 给了我们两个解决方案：神秘的 `StateT` 和 `state` 函数。

`state` 函数其实是在 MonadState 中定义的：

```haskell
class Monad m => MonadState s (m :: * -> *) | m -> s where
  ...
  state :: (s -> (a, s)) -> m a
```

而接下来要讲的，就是神秘的 `StateT`。

## 次时代的 State

想一想，之前用 State 实现的 栈 有什么不足？是的，`pop` 函数无法处理空栈的情况，而是无奈地抛出一个错误，如果只是单纯地组合 State 和 Maybe：

```haskell
push :: a -> State [a] ()
push = modify . (:)
-- 等价于
-- push i = State $ \s -> ((), i:s)

pop :: State [a] (Maybe a)
pop = do
    xs <- get

    case xs of
      [] -> return Nothing
      (x:xs) -> put xs >> return (Just x)
```

看起来没问题，让我们试试看。

```haskell
stack :: State [Int] (Maybe ())
stack = do
  push 1
  v <- pop
  xs <- get

  case v of
    Nothing -> return Nothing
    Just v' -> do
        push (v' + 1)
        v'' <- pop
        xs' <- get

        case v'' of
            Nothing -> return Nothing
            Just v''' -> do
                push $ v''' + 2

                return $ Just ()
```

不忍直视。
会发现又花费了大量代码在处理 Maybe 上，有没有什么办法可以自动处理这个 Maybe 呢？

## 跨时代的 StateT

StateT 是一个 Monad 转换器（Monad Transformer），它可以和其他 Monad 组合，变成更强大的 Monad。

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) } deriving (Functor)
```

和 `State` 的定义很像，只是多了个类型参数 m 而已。但就是因为这个 m，使得 StateT 拥有了和其他 Monad 组合的能力。

```haskell
instance Monad m => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    (StateT m) >>= f = StateT $ \s -> do
        (v, s') <- m s
        let (StateT g) <- f v

        g s'

instance Monad m => Monad (StateT s m) where
    pure = return
    (<*>) = ap
```

定义也是十分相似，接下来看看 StateT 是如何解决重复检查 Nothing 值的问题的。

```haskell
push :: a -> StateT [a] Maybe ()
push = modify . (:)

pop :: StateT [a] Maybe a
pop = do
    xs <- get

    case xs of
        [] -> StateT $ \_ -> Nothing
        (x:xs) -> put xs >> return x
```

甚至实现也和 State 的极度相似，再试试这个 StateT 版本的栈。

```haskell
stack :: StateT [Int] Maybe ()
stack = do
    push 1
    v <- pop

    push $ v + 1
    v <- pop

    push $ v + 2
```

十分优雅和熟悉的使用方式，只不过返回值和 State 的有些不一样：

```haskell
-- State 和 Maybe 组合
> runState stack []
(Just (), [4])
-- StateT 和 Maybe 复合
Just ((), [4])
```

不过，Maybe 似乎也是一个 Monad，有没有 MaybeT 呢？

```haskell
-- 省略 push, pop, stack 的实现，因为除了签名为 MaybeT (State [a]) 以外几乎没有区别

> runState (runMaybeT stack) []
(Just (), [4])
```

返回值和单纯把 State 与 Maybe 组合完全一致了。来看一看 MaybeT 的定义：

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

而在上面的代码中，这里的 m 是 `(State [a])`，则是 `State [a] (Maybe a)`，和单纯组合 State 与 Maybe 的签名一致了，不过多出来的强大功能则是自动处理了 Nothing 值。

## 先有 State 还是先有 StateT

不知道读者知不知道 `Identity` 这个 Monad。

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

`Identity` 只是单纯地包含了一个值，并不做任何运算：

```haskell
> Identity 1 >>= \i -> (Identity i + 2)
Identity 3
> (+2) 1
3
```

如果试着把 StateT 与 Identity 复合，那么我们得到的 `runStateT` 类型将是：`s -> Identity (a, s)`

但 Identity 只是简单地包含一个类型，`s -> Identity (a, s)` 和 `s -> (a, s)` 没有实质上的区别，或者说，这两者是**同构**的。
有好奇心的读者可能会对 State 在 GHCi 使用 `:i` 来查看定义：

```haskell
> :t State
type State s = StateT s Data.Functor.Identity.Identity :: * -> *
```

没错，之前所介绍的 State 就是 StateT 和 Identity 复合的结果。

## 抽象，都可以抽象

像 State（其实是 StateT）有 MonadState 抽象一样，Monad Transformer 也有更高一层抽象的类型类。

```haskell
class MonadTrans (t :: (* -> *) -> * -> *) where
  lift :: Monad m => m a -> t m a
```

`lift` 函数接收一个 Monad，并把它提升（复合）到更高层次的 Monad，比如 StateT 与 Maybe 的复合：

```haskell
fail' :: StateT s Maybe a
-- fail' = StateT $ \_ -> Nothing
fail' = lift Nothing
```

这样，就能把 StateT 和 Maybe 复合的 pop 中的 `Nothing -> StateT $ \_ -> Nothing` 代替为 `Nothing -> lift Nothing`。
不过回头想一想，单纯组合 State 与 Maybe 中，处理 Nothing 值使用的是 `Nothing -> return Nothing`，再看看 `return` 和 `lift` 的类型签名：

```haskell
return ::                Monad m  =>   a ->   m a
lift   :: (MonadTrans t, Monad m) => m a -> t m a
```

`lift` 就像是 Monad Transformer 中的 `return`。

## 最后

State 和 StateT 只是一个 Monad 的冰山一角，在 Haskell 中还有更多未知等待着我们去发现和开拓。

## 注意

所有 `deriving (Functor)` 需要 `DeriveFunctor` 扩展
