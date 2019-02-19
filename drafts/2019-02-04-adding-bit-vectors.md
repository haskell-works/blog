---
title: Adding bit vectors
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
bit-vector recover the carry bits and propagate them.

# Recovering the carry

As mentioned earlier, the carry bit is notionally set when there is an overflow
and an overflow also results in a truncated value that is smaller than at least
one of the addends.

We can therefore determine whether the carry bit should be set by testing for the
latter condition: $$total < a \lor total < b$$

This allows us to write a function that returns both the sum and the carry:

```haskell
sumCarryIncomplete :: Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarryIncomplete a b = (total, carry)
  where total     = a + b
        carry  = if total < a || total < b then 1 else 0
```

This is fine for additing the first two words in our bit-vector, the addition
of following words will need to incorporate the carry.  This ends up being
a threeway addition that includes the carry.

We should therefore extend the function to take $$carry$$ as an `Bool` argument
so that it can also be added to the result ([full source][2]):

```haskell
sumCarry0 :: Word64 -> Word64 -> Bool -> (Word64, Bool)
sumCarry0 a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = if carry then preTotal + 1 else preTotal
        newCarry  = total < a || total < b || (carry && total < 1)
```

Running this code shows that it takes `3.1 seconds` to run, which is fairly slow:

```bash
$ time ex-vector sum-bit-vectors -i ../hw-json/corpus/bench/78mb.json -i ../hw-json/corpus/bench/78mb.json --branchiness branchiest
3.108
```

# First optimisation: Avoiding Bool

We can do better by avoiding the use of `Bool`, which is a sum type.  Use
such types are fairly risky in high-performance code because we would like
for them to be fully optimised away and this doesn't always happen.

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
                  total :: Word64
                  = case w2 of wild {
                      False
                      -> case w of wild1 { W64# x# ->
                        case w1 of wild2 { W64# y# -> W64# (plusWord# x# y#) } }
                      True
                      -> case w of wild1 { W64# x# ->
                        case w1 of wild2 { W64# y# ->
                        W64# (plusWord# (plusWord# x# y#) 1##) } } }
                } in
                (# total,
                  case total of wild { W64# x ->
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
branches in total.

We can avoid the use of an inefficient data type by replacing `Bool` with `Word64`,
`True` with `1` and `False` with `0`.

This simple change also allows us to avoid the `if` expression previously used
to component the $$total$$, so we can expect also the avoid one of the branches
as well ([full source][3]):

```haskell
sumCarry1 :: Wor64 -> Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarry1 a b carry = (total, newCarry)
  where total     = a + b + carry
        newCarry  = if total < a || total < b || total < carry then 1 else 0
```

This results in the following core:

```haskell
$wsumCarry
  = \ w_smut w1_smuu w2_smuv ->
      let {
        total_sk3O
        total_sk3O
          = case w_smut of { W64# x#_aj6l ->
            case w1_smuu of { W64# y#_aj6p ->
            case w2_smuv of { W64# y#1_Xjkx ->
            W64# (plusWord# (plusWord# x#_aj6l y#_aj6p) y#1_Xjkx)
            }
            }
            } } in
      (# total_sk3O,
         case total_sk3O of { W64# x_aj6G ->
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

But before we can do that we're going to need a bit of refactoring.

Our curren implementation detects when an overflow has happened by using the
following test $$total < a \lor total < b \lor total < carry$$.

We might have tried the alternative more intuitive test of
$$total < a + b + carry$$ instead, but sadly that does not
work because as mentioned earlier, we lose the carry bit.

We can however make the observation that unconstrained by word sizes, the test
is valid and we can use this to derive an alternative test that also works.

Jumping back to our first implementation the expression
$$total < a \lor total < b \lor total < carry$$
can be rewritten as $$total < a \verb+ max + b \verb+ max + carry$$, where $$max$$ is a function that
returns the largest argument.  That is to say, the
total is less than any of the other numbers if the total is less than the largest
of them.

We now have two different expressions that test for carry:

* $$total < a \verb+ max + b \verb+ max + carry$$
* $$total < a + b + carry$$

We can then make the observation that the following inequation holds:

$$a \verb+ max + b \verb+ max + carry \leq a \lor b \lor carry  \leq a + b + carry$$

We know the left inequation is true because the largest addend ORed with
any other number will be at least the value of the largest addend.

We know the right inequation to be true because we can consider two
cases.  If there is not overlap in the addends then the LHS and the RHS
end up being equal.  If there is overlap, then the LHS is conceptually
like a sum that has lost those bits where there is overlap in one of the
registers, resulting in a value smaller than that given by an addition.

We can therefore substitute the following test instead which has
fewer instructions: $$total < a \lor b \lor carry$$

This results in the following implementation:

```haskell
sumCarry2 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry2 a b carry = (total, newCarry)
  where total     = a + b + carry
        newCarry  = if total < a .|. b .|. carry then 1 else 0
```

In practise the impact of reducing the implementation by two instructions
is too small to make an observable difference.

Unfortunately both version involve an `if` statement and on modern CPU
architectures because of pipelining, there is a performance penalty for
these kinds of branches.

# Branchless tests

Fortunately, we can avoid the `if` test altogether by making a branchless comparison
using the `ltWord#` primop which will return `1` instead of `0` when the test is true.

For convenience we wrap the primop in an `ltWord` function.

```haskell
{-# LANGUAGE MagicHash #-}

import GHC.Int
import GHC.Prim
import GHC.Word hiding (ltWord)

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
```

Notice the type signature, which is `Word64 -> Word64 -> Word64` instead of
`Word64 -> Word64 -> Bool`.

Such tests are called branchless comparisons.

We can then define a version of `sumCarry3`, the moral equivalent of `sumCarry1`
except without an `if` statement:

```haskell
sumCarry3 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry3 a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = preTotal + carry
        newCarry  = total `ltWord` (a .|. b .|. carry)
```

Or alternatively a `sumCarry4`, the moral equivalent of `sumCarry2`, which has
fewer instructions.

```haskell
sumCarry3 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry3 a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = preTotal + carry
        newCarry  = total `ltWord` (a .|. b .|. carry)
```

# Adding bit-vectors

Now we can use our `sumCarry` function to implement bit-vector
addition by 

For example we show how to add two bit-vectors `a` and `b` in the following
diagram

```text
                ABCDEFGH   IJKLMNOP   QRSTUVWX   YZαβγδεζ
               ┌─────────────────────────────────────────
carry         0│00000000─┬─10000000─┬─10000000─┬─10000000
a = 128       1│00010001─┤ 00011111─┤ 10001000─┤ 00000000
b = 128       2│00010001─┘ 11111000─┘ 00011111─┘ 11000000
a + b         3│00001000   00011000   01010000   00100000
```

Starting in the left-most column, we initialise the carry `0A-0H` to
`0x00`.  We then compute the sum `3A-3H = 0A-0H + 1A-1H + 2A-2H`.

This sum can be computed with the `sumCarry` function:

The `total` is calculated by adding all three words (`a`, `b`, and `carry`)
ignoring overflow.  Separately, the `newCarry` is calculated by comparing
each of the addends to the total.  The reason this works is that adding
three positive numbers together ought result in a number that is at least
any one of them.  If this expectation does not hold, we know overflow has
occurred and therefore the carry is `1`.


We would like a way to recover the carry bit so that it can be added to the
next word in the bit vector.

Firstly we perform bytewise additions:

* `6A..6H = 0A..0H + 1A..1H`
* `6I..6P = 0I..0P + 1I..1P`
* `6Q..6X = 0Q..0X + 1Q..1X`
* `6Y..6ζ = 0Y..0ζ + 1Y..1ζ`

Then we compute the carry to be `0x01` if there is an
overflow or `0x0` otherwise:

* `5I..5P = if overflow (0A..0H + 1A..1H) then 0x01 else 0x00`
* `5Q..5X = if overflow (0I..0P + 1I..1P) then 0x01 else 0x00`
* `5Y..5ζ = if overflow (0Q..0X + 1Q..1X) then 0x01 else 0x00`

The bit-vector sum is then:

With our branchless `sumCarry4` function, we can define the sume of two vectors
(with an initial carry) like so:

```haskell
sumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> Word64 -> (Word64, DVS.Vector Word64)
sumVector u v carry = DVS.createT $ do
  w <- DVSM.new len
  go w 0 0
  return (undefined, w)
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Word64 -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = sumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return c
```

# Benchmarks

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
[2]: https://github.com/haskell-works/blog-examples/blob/03c30c605fe9b5d3ac933e744e2122b3313b7262/ex-vector/src/Ops/SumBitVectors/Branchiest.hs
[3]: https://github.com/haskell-works/blog-examples/blob/f966eb5a58310cb768ae25a55f7c0bb0ce82d4b8/ex-vector/src/Ops/SumBitVectors/Branchy.hs
[4]: https://github.com/haskell-works/blog-examples/blob/master/ex-vector/src/Ops/SumBitVectors/Branchless.hs
