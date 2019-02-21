---
title: Adding bit vectors - Branchless Comparisons
author: John Ky
---

In a [previous post][1], I described how addition can be used flip runs of bits, but
I left the method of adding large bit-vectors unexplained.

This post will fill this gap by explaining how to add large bit-vectors and how
to do so efficiently.

Examples in this post will work with examples that use vectors of `Word8` in
order to be concise in the explanation, whereas the real implementation will
use vectors of `Word64` for efficiency.

As per usual, words when expressed in binary form are expressed in Little Endian.

# Overflows

CPUs perform addition on registers of finite size.  We shall describe the register
in question as having $$n$$ bits indexed from $$0$$ to $$n - 1$$.  When adding
large integers, there inevitably comes a point where the resulting integer requires
more than $$n$$ bits to store and will not fit in the register and an overflow occurs.

During an overflow, the least $$n$$ signifant bits of the result are stored in the
target register resulting in a number that is smaller than at least one of the addends.
The $$n^{th}$$ bit, called the carry bit, would have been set to $$1$$ but due to lack of
register storage is lost instead.

In order to perform additions on bit-vectors of greater than size $$n$$, we will
need to somehow perform repeated additions using register sized slices of the
bit-vector, recover the carry bits and propagate them.

# Recovering the carry

As mentioned earlier, the carry bit is notionally set when there is an overflow
and an overflow also results in a truncated value that is smaller than at least
one of the addends.

We can therefore determine whether the carry bit should be set by testing for the
latter condition: $$partialSum < a \lor partialSum < b$$

This allows us to write a function that returns both the $$partialSum$$ and the
$$carryOut$$:

```haskell
add :: Wor64 -> Wor64 -> (Wor64, Bool)
add a b = (partialSum, carryOut)
  where partialSum     = a + b
        carryOut       = if partialSum < a || partialSum < b then 1 else 0
```

This is fine for adding the first two words in our bit-vector, but the addition
of following pairs of words will need to incorporate the carry.  This ends up
being a threeway addition that includes an input carry.

We can write another function `addCarry` to perform a threeway addition ([full source][2]).

```haskell
addCarry :: Word64 -> Word64 -> Bool -> (Word64, Bool)
addCarry a b c = (t, carry0 || carry1)
  where (s, carry0) = add a b
        (t, carry1) = add s (if c then 1 else 0)
```

`addCarry` is implemented by calling `add` to add the first two numbers and then
converting the carry to a `1` or `0` and adding that in also.

Running this code shows that it takes `3.1 seconds` to run, which is fairly slow:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchiest
3.674
```

# First optimisation: Avoiding Bool

We can do better by avoiding the use of `Bool`, which is a [sum type][5].

Using such types are fairly risky in high-performance code because the complexity
of representing them in memory makes them slow.  Ideally, we would like to
be able to use them and depend on the compiler to fully optimised them away, but
this doesn't always happen.

We can see that GHC has failed to optimise away these constants by looking
at GHC core, which is an intermediate representation used by the compiler
for things such as optimisation.

We can instruct GHC to emit GHC core by invoking it with additional flags:

```bash
stack build --ghc-options="-ddump-simpl -dsuppress-all -dsuppress-coercions"
```

The GHC core for `sumCarry0` is reproduced here:

```haskell
$waddCarry :: GHC.Word.Word64 -> GHC.Word.Word64 -> GHC.Types.Bool -> (# GHC.Word.Word64, GHC.Types.Bool #)
{- Arity: 3, HasNoCafRefs, Strictness: <L,U(U)><L,U(U)><L,1*U>,
    Inline: [0],
    Unfolding: (\ (w :: GHC.Word.Word64)
                  (w1 :: GHC.Word.Word64)
                  (w2 :: GHC.Types.Bool) ->
                let {
                  total :: GHC.Word.Word64
                  = case w of wild { GHC.Word.W64# x# ->
                    case w1 of wild1 { GHC.Word.W64# y# ->
                    GHC.Word.W64# (GHC.Prim.plusWord# x# y#) } }
                } in
                let {
                  b :: GHC.Word.Word64
                  = case w2 of wild {
                      GHC.Types.False -> Ops.SumBitVectors.Word64.Branchiest.addCarry2
                      GHC.Types.True -> Ops.SumBitVectors.Word64.Branchiest.addCarry1 }
                } in
                let {
                  total1 :: GHC.Word.Word64
                  = case total of wild { GHC.Word.W64# x# ->
                    case b of wild1 { GHC.Word.W64# y# ->
                    GHC.Word.W64# (GHC.Prim.plusWord# x# y#) } }
                } in
                (# total1,
                  case total of wild { GHC.Word.W64# x ->
                  case w of wild1 { GHC.Word.W64# y ->
                  case GHC.Prim.ltWord# x y of lwild {
                    DEFAULT
                    -> case w1 of wild2 { GHC.Word.W64# y1 ->
                        case GHC.Prim.ltWord# x y1 of lwild1 {
                          DEFAULT
                          -> case total1 of wild3 { GHC.Word.W64# x1 ->
                            case GHC.Prim.ltWord# x1 x of lwild2 {
                              DEFAULT
                              -> case b of wild4 { GHC.Word.W64# y2 ->
                                  GHC.Prim.tagToEnum# @ GHC.Types.Bool (GHC.Prim.ltWord# x1 y2) }
                              1# -> GHC.Types.True } }
                          1# -> GHC.Types.True } }
                    1# -> GHC.Types.True } } } #)) -}
```

We can see from the above dump that the `addCarry` function has nicely inlined away
all the calls to `add`.  We can also observe the use of `True` and `False` on lines
 values on lines `16`, `17`, `38`, `39`, and `40`.

Moreover, we can count the number of branch instructions
in the core by looking at `case` statements that have at least two branches.

Such case statements can be identified on lines `15`, `28`, `31`, and `34`,
adding up to `4` branches in `addCarry`.

We can avoid the use of the inefficient data type by replacing `Bool` with `Word64`,
`True` with `1` and `False` with `0`.

This simple change also allows us to avoid the `if` expression previously used
to component the $$partialSum$$, so we can expect also the avoid one of the branches
as well ([full source][3]):

```haskell
add :: Word64 -> Word64 -> (Word64, Word64)
add a b = (total, newCarry)
  where total     = a + b
        newCarry  = if total < a || total < b then 1 else 0

addCarry :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
addCarry a b c = (t, carry0 .|. carry1)
  where (s, carry0) = add a b
        (t, carry1) = add s c
```

This results in the following core:

```haskell
  $waddCarry ::
    GHC.Word.Word64
    -> GHC.Word.Word64
    -> GHC.Word.Word64
    -> (# GHC.Word.Word64, GHC.Word.Word64 #)
  {- Arity: 3, HasNoCafRefs, Strictness: <L,U(U)><L,U(U)><L,U(U)>,
     Inline: [0],
     Unfolding: (\ (w :: GHC.Word.Word64)
                   (w1 :: GHC.Word.Word64)
                   (w2 :: GHC.Word.Word64) ->
                 let {
                   total :: GHC.Word.Word64
                   = case w of wild { GHC.Word.W64# x# ->
                     case w1 of wild1 { GHC.Word.W64# y# ->
                     GHC.Word.W64# (GHC.Prim.plusWord# x# y#) } }
                 } in
                 let {
                   total1 :: GHC.Word.Word64
                   = case total of wild { GHC.Word.W64# x# ->
                     case w2 of wild1 { GHC.Word.W64# y# ->
                     GHC.Word.W64# (GHC.Prim.plusWord# x# y#) } }
                 } in
                 (# total1,
                    case total of wild { GHC.Word.W64# x ->
                    case w of wild1 { GHC.Word.W64# y ->
                    let {
                      $j :: GHC.Prim.Word# -> GHC.Word.Word64
                        <join 1> {- Arity: 1, Strictness: <S,U>m -}
                      = \ (x# :: GHC.Prim.Word#)[OneShot] ->
                        case total1 of wild2 { GHC.Word.W64# x1 ->
                        case GHC.Prim.ltWord# x1 x of lwild {
                          DEFAULT
                          -> case w2 of wild3 { GHC.Word.W64# y1 ->
                             case GHC.Prim.ltWord# x1 y1 of lwild1 {
                               DEFAULT -> GHC.Word.W64# x#
                               1# -> GHC.Word.W64# (GHC.Prim.or# x# 1##) } }
                          1# -> GHC.Word.W64# (GHC.Prim.or# x# 1##) } }
                    } in
                    case GHC.Prim.ltWord# x y of lwild {
                      DEFAULT
                      -> case w1 of wild2 { GHC.Word.W64# y1 ->
                         case GHC.Prim.ltWord# x y1 of lwild1 {
                           DEFAULT -> $j 0## 1# -> $j 1## } }
                      1# -> $j 1## } } } #)) -}
```

The core shows three branching `case` expressions on lines `31`, `34` and `39`.

Let's see how this performs:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchy
1.334
```

That works out to be a performance boost of about `63.7%`.

# Branchless tests

We can do a little bit better by further trying to reduce the number of branches
in our code.

Let's zoom in to a smaller section of the core we saw before:

```haskell
  (# total1,
    case total of wild { GHC.Word.W64# x ->
    case w of wild1 { GHC.Word.W64# y ->
    let {
      $j :: GHC.Prim.Word# -> GHC.Word.Word64
        <join 1> {- Arity: 1, Strictness: <S,U>m -}
      = \ (x# :: GHC.Prim.Word#)[OneShot] ->
        case total1 of wild2 { GHC.Word.W64# x1 ->
        case GHC.Prim.ltWord# x1 x of lwild {
          DEFAULT
          -> case w2 of wild3 { GHC.Word.W64# y1 ->
              case GHC.Prim.ltWord# x1 y1 of lwild1 {
                DEFAULT -> GHC.Word.W64# x#
                1# -> GHC.Word.W64# (GHC.Prim.or# x# 1##) } }
          1# -> GHC.Word.W64# (GHC.Prim.or# x# 1##) } }
    } in
    case GHC.Prim.ltWord# x y of lwild {
      DEFAULT
      -> case w1 of wild2 { GHC.Word.W64# y1 ->
          case GHC.Prim.ltWord# x y1 of lwild1 {
            DEFAULT -> $j 0## 1# -> $j 1## } }
      1# -> $j 1## } } } #)) -}
```

On lines `9`, `12` and `20` are mysterious calls to a function called `ltWord#`.

The `ltWord#` function is actually what we call a primop:  A special function that
is implemented internally by GHC.  The `ltWord#` primop behaves like `(<)` except
that it will return `1#` instead of `True` and `0#` instead of `False`.

We can find the this function in the [docs][7] as having the type `Word# -> Word# -> Int#`

Notice that the primop uses the types `Word#` and `Int#`.  These are
[unboxed values][6].  These types have less overhead than the boxed types
like `Word64` or `Bool`, and GHC will optimise boxed types to unboxed types
when it believes it can safely do so.

Our performance problem lies in the fact that we whenever our compiled code calls
`ltWord#` it tests against the return value for `DEFAULT` (ie. `0#`) and `1#`, whereas
if we recognised that it was an integer and used it in arithmetic directly, we can
avoid the branch.

But because it uses unboxed types, calling it directly would be inconvenient
so we write a wrapper that uses boxed types and depend on GHC to optimise
away our pessimisation:

```haskell
{-# LANGUAGE MagicHash #-}

import GHC.Int
import GHC.Prim
import GHC.Word hiding (ltWord)

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
```

Notice the type signature we've chosen to use, which is
`Word64 -> Word64 -> Word64` instead of the `Word64 -> Word64 -> Bool`
of the `(<)` operator.

Such tests are called branchless comparisons because a branch is not
required to use the result of the comparison.

We can then define a version of `add`, the moral equivalent of `sumCarry1`
except without an `if` statement:

```haskell
add :: Word64 -> Word64 -> (Word64, Word64)
add a b = (total, newCarry)
  where total     = a + b
        newCarry  = total `ltWord` a || total `ltWord` b
```

Running this version of the code yields run in less time again:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchless
1.005
```

This optimisation shaves another `9.0%` of the runtime bringing the savings to a partialSum of `72.6%`.

# Closing Remarks

In this blog post implemented the `sumCarry` and explored different implementations and
evaluated various optimisation strategies such uses avoiding the boxed type `Bool`
and using branchless comparisons to reduce the number of branches in the optimised code.

All that remains in order to perform a bit-vector addition is to invoke the `sumCarry`
function over all the words in the intput bit-vectors, ensuring that carries are
propagated properly between adjacent additions.

A future blog post will look at how we can write such a function in a pure functional
setting where we require mutation to populate the result bit-vector, but still want
to retain the safety that immutability affords.

# Appendix

## Summary of Benchmarks

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchiest
3.674

$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchy
1.334

$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchless
1.005
```

The source code for the above benchmarks can be found in the
[Branchiest][2], [Branchy][3], and [Branchless][4] modules.

# References

[1]: ../posts/2018-08-15-data-parallel-rfc-compliant-csv-parsing.html
[2]: https://github.com/haskell-works/blog-examples/blob/99b45428f43e7428383cbe4e4f1d53c38cd830d0/ex-vector/src/Ops/SumBitVectors/Word64/Branchiest.hs
[3]: https://github.com/haskell-works/blog-examples/blob/99b45428f43e7428383cbe4e4f1d53c38cd830d0/ex-vector/src/Ops/SumBitVectors/Word64/Branchy.hs
[4]: https://github.com/haskell-works/blog-examples/blob/99b45428f43e7428383cbe4e4f1d53c38cd830d0/ex-vector/src/Ops/SumBitVectors/Word64/Branchless.hs
[5]: https://wiki.haskell.org/Algebraic_data_type
[6]: https://wiki.haskell.org/Unboxed_type
[7]: https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#v:ltWord-35-
