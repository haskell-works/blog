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
sumCarryIncomplete :: Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarryIncomplete a b = (partialSum, carryOut)
  where partialSum     = a + b
        carryOut       = if partialSum < a || partialSum < b then 1 else 0
```

This is fine for adding the first two words in our bit-vector, but the addition
of following pairs of words will need to incorporate the carry.  This ends up
being a threeway addition that includes an input carry.

We should therefore extend the function to take $$carryIn$$ as an `Bool` argument
so that it can also be added to the result ([full source][2]):

```haskell
sumCarry0 :: Word64 -> Word64 -> Bool -> (Word64, Bool)
sumCarry0 a b carryIn = (partialSum, carryOut)
  where prePartialSum = a + b
        partialSum    = if carryIn then prePartialSum + 1 else prePartialSum
        carryOut      = partialSum < a || partialSum < b || (carryIn && partialSum < 1)
```

This means that instead of relying on $$partialSum < a \lor partialSum < b$$ we will
need to rely on $$partialSum < a \lor partialSum < b \lor partialSum < carryIn$$ as a reliable
test for overflow, which fortunately still works.

Running this code shows that it takes `3.1 seconds` to run, which is fairly slow:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchiest
3.108
```

# First optimisation: Avoiding Bool

We can do better by avoiding the use of `Bool`, which is a [sum type][5].

Using such types are fairly risky in high-performance code because the complexity
of representing them in memory makes them slow.  Ideally, we would like to
be able to use them and depend on the compiler to fully optimised away, but
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
$wsumCarry0 :: Word64 -> Word64 -> Bool -> (# Word64, Bool #)
{- Arity: 3, HasNoCafRefs, Strictness: <L,U(U)><L,U(U)><L,U>,
    Inline: [0],
    Unfolding: (\ (w :: Word64) (w1 :: Word64) (w2 :: Bool) ->
                let {
                  partialSum :: Word64
                  = case w2 of wild {
                      False
                      -> case w of wild1 { W64# x# ->
                        case w1 of wild2 { W64# y# -> W64# (plusWord# x# y#) } }
                      True
                      -> case w of wild1 { W64# x# ->
                        case w1 of wild2 { W64# y# ->
                        W64# (plusWord# (plusWord# x# y#) 1##) } } }
                } in
                (# partialSum,
                  case partialSum of wild { W64# x ->
                  case w of wild1 { W64# y ->
                  case ltWord# x y of lwild {
                    DEFAULT
                    -> case w1 of wild2 { W64# y1 ->
                        case ltWord# x y1 of lwild1 {
                          DEFAULT
                          -> case w2 of wild3 {
                              False -> False
                              True -> tagToEnum# @ Bool (ltWord# x 1##) }
                          1# -> True } }
                    1# -> True } } } #)) -}
```

From the dump, we can see the use of `True` and `False` values on lines `8`,
`11`, `25`, and `26`.

Moreover, we can count the number of branch instructions
in the core by looking at `case` statements that have at least two branches.
These can be identified on lines  `7`, `19`, `22`, and `24`, adding up to `4`
branches in partialSum.

We can avoid the use of the inefficient data type by replacing `Bool` with `Word64`,
`True` with `1` and `False` with `0`.

This simple change also allows us to avoid the `if` expression previously used
to component the $$partialSum$$, so we can expect also the avoid one of the branches
as well ([full source][3]):

```haskell
sumCarry1 :: Wor64 -> Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarry1 a b carryIn = (partialSum, newCarry)
  where partialSum    = a + b + carryIn
        newCarry      = if partialSum < a || partialSum < b || partialSum < carryIn then 1 else 0
```

This results in the following core:

```haskell
$wsumCarry
  = \ w_smut w1_smuu w2_smuv ->
      let {
        partialSum_sk3O
        partialSum_sk3O
          = case w_smut of { W64# x#_aj6l ->
            case w1_smuu of { W64# y#_aj6p ->
            case w2_smuv of { W64# y#1_Xjkx ->
            W64# (plusWord# (plusWord# x#_aj6l y#_aj6p) y#1_Xjkx)
            }
            }
            } } in
      (# partialSum_sk3O,
         case partialSum_sk3O of { W64# x_aj6G ->
         case w_smut of { W64# y_aj6K ->
         case ltWord# x_aj6G y_aj6K of {
           __DEFAULT ->
             case w1_smuu of { W64# y1_XjaA ->
             case ltWord# x_aj6G y1_XjaA of {
               __DEFAULT ->
                 case w2_smuv of { W64# y2_XjaJ ->
                 case ltWord# x_aj6G y2_XjaJ of {
                   __DEFAULT -> sumCarry2;
                   1# -> sumCarry1
                 }
                 };
               1# -> sumCarry1
             }
             };
           1# -> sumCarry1
         }
         }
         } #)
```

The core shows three branching `case` expressions on lines `16`, `19` and `22`.

Assuming we have all the additional code necessary to call our function and
perform the bit-vector addition (to be described later in
[Adding bit-vectors](#adding-bit-vectors)), let's see how this performs:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchy
1.330
```

That works out to be a performance boost of about `57%`.

# A little refactoring

We can do a little bit better by further trying to reduce the number of branches
in our code.

But before we can do that we're going to explore a bit of refactoring.

Our earlier implementation detects when an overflow has happened by using the
following test $$partialSum < a \lor partialSum < b \lor partialSum < carryIn$$.

We might have tried the alternative more intuitive test of
$$partialSum < a + b + carryIn$$ instead, but sadly that does not
work because as mentioned earlier, we lose the carry bit.

We can however make the observation that unconstrained by word sizes, the test
is valid and we can use this to derive an alternative test that also works.

Jumping back to our first implementation the expression
$$partialSum < a \lor partialSum < b \lor partialSum < carryIn$$
can be rewritten as $$partialSum < a \verb+ max + b \verb+ max + carryIn$$,
where $$\verb+max+$$ is a function that
returns the largest argument.  That is to say, the
partialSum is less than any of the other numbers if the partialSum is less than the largest
of them.

We now have two different expressions that test for overflow:

* $$partialSum < a \verb+ max + b \verb+ max + carryIn$$
* $$partialSum < a + b + carryIn$$

We can then make the observation that the following inequality holds:

$$a \verb+ max + b \verb+ max + carryIn \leq a + b + carryIn$$

Despite them both being valid tests for overflow, they are not necessarily
equal.  For example when any of the values $$a$$, $$b$$ or $$carryIn$$ differ
from each other then the expressions are not equal, but in this case, the
left is less than the right.

We can then devise a simpler expression that we can prove evaluates to a result
that lies between the two extremes and being bounded by those other expression
means that it also constitutes a valid test for overflow.

$$a \lor b \lor carryIn$$ is one such expression.

This means the following inequality should hold:

$$a \verb+ max + b \verb+ max + carryIn \leq a \lor b \lor carryIn \leq a + b + carryIn$$

We know the left inequality is true because the largest addend ORed with
any other number will be at least the value of the largest addend.

We know the right inequality to be true because we can consider two
cases.  If there is no overlap in the bit patterns of the addends then
the LHS and the RHS end up being equal.  If there is overlap, then the LHS
is conceptually like a sum that has lost those bits where there is overlap
in one of the registers, resulting in a value smaller than that given by an
actual addition.

We can therefore substitute the following test instead which has
fewer instructions: $$partialSum < a \lor b \lor carryIn$$

This results in the following implementation:

```haskell
sumCarry2 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry2 a b carryIn = (partialSum, carryOut)
  where partialSum  = a + b + carryIn
        carryOut    = if partialSum < a .|. b .|. carryIn then 1 else 0
```

In practise the impact of reducing the implementation by two instructions
is too small to make an observable difference.

But the latter technique is used in the [`hw-dsv`][6] library, so it
needed some explanation.

Unfortunately both version involve an `if` statement and on modern CPU
architectures because of pipelining, there is a performance penalty for
these kinds of branches.  We would like to be able to avoid them if we
can.

# Branchless tests

Let's look back a snippet of core from the previous dump:

```haskell
  case ltWord# x_aj6G y_aj6K of {
    __DEFAULT ->
      case w1_smuu of { W64# y1_XjaA ->
      case ltWord# x_aj6G y1_XjaA of {
        __DEFAULT ->
          case w2_smuv of { W64# y2_XjaJ ->
          case ltWord# x_aj6G y2_XjaJ of {
            __DEFAULT -> sumCarry2;
            1# -> sumCarry1
          }
          };
        1# -> sumCarry1
      }
      };
    1# -> sumCarry1
```

One line `1`, we can see our `sumCarry1` is calling an `ltWord#` function that does
not exist in our definition.  This is because the `(<)` operator we do use calls
`ltWord#` and GHC has inlined our use of the operator.

The `ltWord#` function is actually what we call a primop.  A special function that
is implemented internally by GHC.  The `ltWord#` primop behaves like `(<)` except
that it will return `1` instead of `True` and `0` instead of `False`.

Let's check out its type:

```bash
$ stack repl ex-vector:exe:ex-vector
λ> :set -XMagicHash
λ> import GHC.Prim
λ> :t ltWord#
ltWord# :: Word# -> Word# -> Int#
```

Notice that the primop uses the types `Word#` and `Int#`.  These are
[unboxed values][6], these types have less overhead than the boxed types
like `Word64` or `Bool`, and GHC will optimise boxed types to unboxed types
when it believes it can safely do so.

What we would like to do is to use the return value of `ltWord#` directly
rather than test on the result and use values that are going to end up the
same anyway.

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
`Word64 -> Word64 -> Word64` instead of `Word64 -> Word64 -> Bool`.

Such tests are called branchless comparisons.

We can then define a version of `sumCarry3`, the moral equivalent of `sumCarry1`
except without an `if` statement:

```haskell
sumCarry3 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry3 a b carryIn = (partialSum, carryOut)
  where prePartialSum = a + b
        partialSum    = prePartialSum + carryIn
        carryOut      = partialSum `ltWord` (a .|. b .|. carryIn)
```

Or alternatively a `sumCarry4`, the moral equivalent of `sumCarry2`, which has
fewer instructions.

```haskell
sumCarry3 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry3 a b carryIn = (partialSum, carryOut)
  where prePartialSum = a + b
        partialSum    = prePartialSum + carryIn
        carryOut      = partialSum `ltWord` (a .|. b .|. carryIn)
```

This optimisation shaves another `8.5%` of the runtime bringing the savings to a partialSum of `66.7%`.

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchless
1.065
```

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
3.108

$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchy
1.330

$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchless
1.065
```

The source code for the above benchmarks can be found in the
[Branchiest](https://github.com/haskell-works/blog-examples/blob/master/ex-vector/src/Ops/SumBitVectors/Branchiest.hs),
[Brancy](https://github.com/haskell-works/blog-examples/blob/master/ex-vector/src/Ops/SumBitVectors/Branchy.hs), and
[Branchless](https://github.com/haskell-works/blog-examples/blob/master/ex-vector/src/Ops/SumBitVectors/Branchless.hs)
modules.

# Closing Remarks

This post looked at how we can resegment our lazy bytestring to make the chunk sizes compatible with
SIMD instructions at a reasonable cost.

The next post will look at using FFI to call into C functions that use SIMD to do the heavy lifting.

[1]: ../posts/2018-08-15-data-parallel-rfc-compliant-csv-parsing.html
[2]: https://github.com/haskell-works/blog-examples/blob/dd02285e7ab791ec1d294cedd8affd774835ebbc/ex-vector/src/Ops/SumBitVectors/Branchiest.hs
[3]: https://github.com/haskell-works/blog-examples/blob/dd02285e7ab791ec1d294cedd8affd774835ebbc/ex-vector/src/Ops/SumBitVectors/Branchy.hs
[4]: https://github.com/haskell-works/blog-examples/blob/dd02285e7ab791ec1d294cedd8affd774835ebbc/ex-vector/src/Ops/SumBitVectors/Branchless.hs
[5]: https://wiki.haskell.org/Algebraic_data_type
[6]: https://wiki.haskell.org/Unboxed_type
