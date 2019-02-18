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

CPUs perform addition on registers of finite size (in number of bits we will call
`n`) so when adding large integers, there inevitably comes a point where the
resulting integer requires more than `n` bits and will not fit in the register
and an overflow occurs.

During an overflow, the least `n` signifant bits of the result are in the target
register resulting in a number that is smaller than one of the addends.  The
`n`th bit, called the carry bit, would have been set to `1` but due to lack of
stroage is lost instead.

In order to perform additions on bit-vectors of greater than size `n`, we will
need to somehow perform repeated additions using register sized slices of the
bit-vector recover the carry bits and propagate them.

# Recovering the carry

As mentioned earlier, the carry bit is notionally set when there is an overflow
and an overflow also results in a truncated value that is smaller than at least
one of the addends.

We can therefore determine whether the carry bit should be set by testing for the
latter condition: `total < a || total < b`

This allows us to write a function that returns both the sum and the carry

```haskell
sumCarry0 :: Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarry0 a b = (total, carry)
  where total     = a + b
        carry  = if total < a || total < b then 1 else 0
```

This is fine for additing the first two words in our bit-vector, the addition
of following words will need to incorporate the carry.  This ends up being
a threeway addition that includes the carry.

We should therefore extend the function to take `carry` as an argument:

```haskell
sumCarry1 :: Wor64 -> Wor64 -> Wor64 -> (Wor64, Wor64)
sumCarry1 a b carry = (total, newCarry)
  where total     = a + b + carry
        newCarry  = if total < a || total < b || total < carry then 1 else 0
```

The alternative more intuitive formulation of `total < a + b + carry` does not
work because the fact that `a + b + carry` overflows means the test is invalid.

We can however make the observation that unconstrained by word sizes, the test
is valid and we can use this to derive an alternative test that works.

Jumping back to our first implementation the expression `total < a || total < b || total < carry`
can be rewritten as `total < (a ``max`` b ``max`` carry)`.  That is to say, the
total is less than any of the other numbers if the total is less than the largest
of them.

We now have two different expressions that test for carry:

* `total < (a ``max`` b ``max`` carry)`
* `total < a + b + carry`

We can then make the observation that the following inequation holds:

`a ``max`` b ``max`` carry  <=  a .|. b .|. carry  <=  a + b + carry`

We know the left inequation is true because the largest addend ORed with
any other number will be at least the value of the largest addend.

We know the right inequation to be true because we can consider two
cases.  If there is not overlap in the addends then the LHS and the RHS
end up being equal.  If there is overlap, then the LHS is conceptually
like a sum that has lost bits where there is overlap in one of the
registers, resulting in a smaller value.

We can therefore substitute the following test instead which has
fewer instructions.

Ths results in the following implementation:

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
without an `if` statement:

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

# Closing Remarks

This post looked at how we can resegment our lazy bytestring to make the chunk sizes compatible with
SIMD instructions at a reasonable cost.

The next post will look at using FFI to call into C functions that use SIMD to do the heavy lifting.

[1]: ../posts/2018-08-15-data-parallel-rfc-compliant-csv-parsing.html



[2]: http://hackage.haskell.org/package/bytestring
[3]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.Internal.html#defaultChunkSize
[4]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html#v:toChunks
[5]: http://hackage.haskell.org/package/hw-prim-0.6.2.15/docs/HaskellWorks-Data-ByteString.html#v:rechunk
[6]: http://hackage.haskell.org/package/hw-prim-0.6.2.15/docs/HaskellWorks-Data-ByteString.html#v:resegment
[7]: http://hackage.haskell.org/package/hw-prim-0.6.2.15/docs/HaskellWorks-Data-ByteString.html#v:resegmentPadded
[8]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Lazy.html#v:hGetContents
[9]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.html#hGetContentsN
[10]: http://hackage.haskell.org/package/base-4.11.1.0/docs/System-IO.html#v:hGetBuf
[11]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Internal.html#createAndTrim
[12]: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.html#hGetSome
[13]: http://hackage.haskell.org/package/hw-prim-0.6.2.17/docs/HaskellWorks-Data-ByteString-Lazy.html#v:hGetContentsChunkedBy
