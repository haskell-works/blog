---
title: Bit-manipulation operations for high-performance succinct data-structures and CSV parsing
author: John Ky
---

In last week's [post][1] I described how to produce rank-select bit-strings for
an RFC-compliant CSV format without detailing how to perform efficiently the
operation that splits odd and even bits of a bit-string into separate bit-strings.

In an earlier [post][2] I discussed `rank` and `select` operations, but also
omitted to describe how they can be implemented efficiently.

In this blog post I will properly introduce the `popcnt`, `pext`, `tzcnt` and
`pdep` operations and how they relate to the performance of our conceptual
succinct data-structure based CSV parser.

# Bit-manipulation instructions

# Pop Count

The `popcnt` operation is quite straight-forward.  It takes an integer as its input argument,
counts the number of 1-bits and produces an integer containing the count.

In the following example, I count the number of 1-bits in my bit-string and find there are 5:

```haskell
~/wrk/haskell-works/hw-rankselect-base $
$ stack repl
λ> import Data.Word
λ> let bs = fromJust $ bitRead "01110101" :: Word8
λ> popCount1 bs
5
```

# Trailing Zeros Count

The `tzcnt` operation is also quite straight-forward.  It counts the number of "trailing
zeros" in the bit-string.

Unfortunately, because we are on the topic of succinct data-structures,
all our bits are expressed in Little-Endian, so visually we are looking at the leading zeros.

In the following example, I count the number of trailing zeros in my bit-string and find there
are 3 (as indicated by the `***` annotation):

```haskell
~/wrk/haskell-works/hw-rankselect-base $
$ stack repl
λ> import Data.Word             ***
λ> let bs = fromJust $ bitRead "00010101" :: Word8
λ> countTrailingZeros bs
3
```

# Parallel Extract

The `pext` or "parallel extract" operation , is an operation that takes a bit-mask and extracts
the bits from the `source` word corresponding to 1-bits in the bit-mask and packs them into
least-significant side of the word.

```text
* All words in Little-Endian *

mask   0011001100110011
source 0110101110101001
bits     10  11  10  01
       ┌─┘│  ││  ││  ││
       │┌─┘  ││  ││  ││
       ││┌───┘│  ││  ││
       │││┌───┘  ││  ││
       ││││┌─────┘│  ││
       │││││┌─────┘  ││
       ││││││┌───────┘│
       │││││││┌───────┘
result 1011100100000000
```

In the following example, I extract the high nibble of each byte in my `Word32` by
using a mask where all the high-nibble bits are set to `1`:

```haskell
~/wrk/haskell-works/hw-rankselect-base $
$ stack repl
λ> import Data.Word
λ> import Data.Bits.Pext
λ> let source = fromJust $ bitRead "01110101 11001010 00001001 01011000" :: Word32
λ> let mask   = fromJust $ bitRead "00001111 00001111 00001111 00001111" :: Word32
λ> bitShow $ pext source mask
"01011010 10011000 00000000 00000000"
```

# Parallel Deposit

The `pdep` or "parallel deposit" operation, is an operation that takes a bit-mask and deposits
the least-significant n-bits from the `source` word where `n` is the number of bits in the bit-mask
and deposits them at the positions marked by 1-bits in the bit-mask.

```text
* All words in Little-Endian *

mask   0011001100110011
source 1011100100000000
       │││││││└───────┐
       ││││││└───────┐│
       │││││└─────┐  ││
       ││││└─────┐│  ││
       │││└───┐  ││  ││
       ││└───┐│  ││  ││
       │└─┐  ││  ││  ││
       └─┐│  ││  ││  ││
bits     10  11  10  01
bits   0010001100100001
```

In the following example, I deposit the first eight bits in my bits-string into the
high nibble of each byte in my `Word32`, again by
using a mask where all the high-nibble bits are set to `1`:

```haskell
~/wrk/haskell-works/hw-rankselect-base $
$ stack repl
λ> import Data.Word
λ> import Data.Bits.Pdep
λ> let source = fromJust $ bitRead "01011010 10011000 00000000 00000000" :: Word32
λ> let mask   = fromJust $ bitRead "00001111 00001111 00001111 00001111" :: Word32
λ> bitShow $ pdep source mask
"00000101 00001010 00001001 00001000"
```

# Availability on GHC

In Haskell, the `popCount` operation is available via the [`popCount`][3] function in `Data.Bits`
module of the [base][4] package.

On most modern x86 systems, the `popCount` operation is available on 64-bit integers in the
form of the [`POPCNT`][5] CPU instruction, so it is very fast.

It is also available to C programs
by way of the `_popcnt64` CPU intrinsic, but what about Haskell?

To confirm that the function does in fact compile down to an instruction we first check the
[definition][6] here.

```haskell
popCount (W# x#)         = I# (word2Int# (popCnt# x#))
```

and notice the use of the primop `popCnt#`.

Digging further into the [GHC source code][7]:

```haskell
    sse4_2 <- sse4_2Enabled
    let platform = targetPlatform dflags
    if sse4_2
      then
                      ....
          unitOL (MOVZxL II8 (OpReg src_r) (OpReg src_r)) `appOL`
          unitOL (POPCNT II16 (OpReg src_r) dst_r)
```

We find that this primop generates the `POPCNT`
instruction provided that `sse4_2Enabled` yields `True`, which according to the [GHC User Guide][8]
can be switched on with the `-msse4.2` `ghc` flag since GHC 7.4.1 on x86 CPU architectures.

A similar search can be done for the `tzcnt` operation, but [unfortunately the equivalent
primop expands to a whole bunch of instructions][10]:

```haskell
([ MOV      II32 (OpReg rhi)         (OpReg tmp_r)
 , OR       II32 (OpReg rlo)         (OpReg tmp_r)
 , MOV      II32 (OpImm (ImmInt 64)) (OpReg dst_r)
 , JXX EQQ    lbl2
 , JXX ALWAYS lbl1
 
 , NEWBLOCK   lbl1
 , BSF     II32 (OpReg rhi)         dst_r
 , ADD     II32 (OpImm (ImmInt 32)) (OpReg dst_r)
 , BSF     II32 (OpReg rlo)         tmp_r
 , CMOV NE II32 (OpReg tmp_r)       dst_r
 , JXX ALWAYS lbl2
 
 , NEWBLOCK   lbl2
])
```

The operation is emulated with `BSF` or Bit-Scan-Forward, which does something similar, but only
works on 32-bit integers.

Unfortunately for me, I needed optimised `pdep` and `pext` primops from `ghc` for my succinct
data-structure libraries, but sadly they weren't available at the time I sought them, which
was about this time last year.

I created a [trac issue][11], and one thing led to another and I ended up [implementing these
primops][12] in `ghc` over the space of several months with the kind help of
[Ben Gamari](https://twitter.com/bgamari) and [Moritz Angermann](https://twitter.com/angerman_io).

The functionality is not exposed in `base`, but they can be accessed from the Haskell-Works
[bits-extra][14] library.  Benchmark results can be found there-in.

If anyone is looking to jump into GHC development, a good first project that can improve the performance
of succinct data-structures on Haskell code would be to add native support for [`ctz#`][13] primop in GHC.

GHC needs more contributions and it would be great to see GHC become a better platform for
writing high performance code.

# Applications

## Very fast implementation of Rank and Select

About this time last year, [Ed Kmett](https://twitter.com/kmett), sent me very interesting
paper on [A General-Purpose Counting Filter: Making Every Bit Count][9].

![Kmett nicely dropping me a paper on Quotient Filters and walking away](../images/kmett-quotient-filter.png)

To really miss the amazing contribution of the paper and zoom in on the section relevant
to this discussion go to section 3.2 "Fast x86 rank and select", which describes how the
`PDEP` and `TZCNT` instructions can be used to implement fast rank and select.

The formula for rank is given in the paper as:

<blockquote>
$$RANK(v, i) = POPCOUNT(v \& (2^i - 1))$$
</blockquote>

The `rank i` of a bit-string is the prefix-sum of the bit-string of the given length `i`.

The diagram below shows five examples of rank operations on the same bit-string (b).

For the operation `rank i`, the expression `2ⁱ-1` computes a bit-mask (a) consisting
of a prefix of 1-bits of
length `i` that can be used to zero in our bit-string (b)
all the bits beyond the prefix length to produce (c).

A pop count of (c) counts
all the 1-bits annotated by `*` (d) and becomes our rank (e).

```text
        rank 2            rank 4            rank 6            rank 8            rank 10         
(a)     1100000000000000  1111000000000000  1111110000000000  1111111100000000  1111111111000000
(b)     0100100010011000  0100100010011000  0100100010011000  0100100010011000  0100100010011000
(c)     0100000000000000  0100000000000000  0100100000000000  0100100000000000  0100100010000000
(d)      *                 *                 *  *              *  *              *  *   *       
(e)      1                 1                 2                 2                 3
```

<blockquote>
  $$SELECT(v, i) = TZCNT(PDEP(2^i, v))$$
</blockquote>

The `select i` of a bit-string is length of the smallest prefix that includes `i` 1-bits
in the bit-string.  Unfortunately, I feel like there is an off-by-one error in the formula
and that it should actually be:

<blockquote>
  $$select(v, i) = tzcnt(pdep(2^{i-1})) + 1$$
</blockquote>

The diagram below shows five examples of select operations on the same bit-string (b).

For the operation `select i`, the expression `2`<sup>`i-1`</sup> computes a bit-string
containing exactly one 1-bit at the i-th position in the bit-string (1-based). 

The `pdep` operation is called to deposit this bit at the position corresponding to the
n-th 1-bit in (b) to produce the result (c).

The number of trailing zeros (indicated by the `*`) when added to one yields the value of `select i`.

```text
           select 1          select 2          select 3         select 4         select 5        
(a) 1000000000000000  0100000000000000  0010000000000000 0010000000000000 0010000000000000
    ││││└───────┐     ││││└───────┐     ││││└───────┐    ││││└───────┐    ││││└───────┐   
    │││└────┐   │     │││└────┐   │     │││└────┐   │    │││└────┐   │    │││└────┐   │   
    ││└───┐ │   │     ││└───┐ │   │     ││└───┐ │   │    ││└───┐ │   │    ││└───┐ │   │   
    │└─┐  │ │   │     │└─┐  │ │   │     │└─┐  │ │   │    │└─┐  │ │   │    │└─┐  │ │   │   
(b) 1001001010001000  1001001010001000  1001001010001000 1001001010001000 1001001010001000
(c) 1000000000000000  0001000000000000  0000001000000000 0000000010000000 0000000000001000
(d) 1                 *** 4             ****** 7         ******** 9       ************ 13
```

## Splitting a bit-string to odd and even bits

In last week's [post][1], I explained that in order to create the rank-select bit-strings
for an RFC compliant CSV format, I needed an operation that can take a bit-string and
collect all the odd bits into one bit-string and all the even-bits into another.

The example I gave is reproduced here:

```text
text:     aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
quotes:   00000000000001011001010000010100001
parens:   0000000000000(0)(00)0(00000)0(0000)
enters:   00000000000001001000010000000100000
exits:    00000000000000010001000000010000001
```

In this post I will be using `odds` in place of `enters` because these are the `odd` bits
and `evens` in place of `exits` because these are the `even` bits in our bit-string.

The opening parentheses `(` represents all the 1-bits that opening quotes of a quoted
string and the closing parentheses `)`
represents all the 1-bits that represents for the closing quotes.

We need to build `odds` which has all the odd 1-bits (ie. the opening quotes) and `leaves`
which has all the even 1-bits (ie. the closing quotes).

We must somehow be able to produce the bit-strings `odds` and `evens` from the
bit-string `quotes` very efficiently.

This is actually very easy with a single application of the `pdep` operation for each bit-string
we produce:

```haskell
~/wrk/haskell-works/hw-rankselect-base $

$ stack repl
λ> import Data.Bits.Pdep
λ> let bs    = fromJust $ bitRead "00000000000001011001010000010100001000000000" :: Word64
λ> let odds  = fromJust $ bitRead "10101010101010101010101010101010101010101010" :: Word64
λ> let evens = fromJust $ bitRead "01010101010101010101010101010101010101010101" :: Word64
λ> bitShow bs
"00000000 00000101 10010100 00010100 00100000 00000000 00000000 00000000"
λ> bitShow $ pdep odds bs
"00000000 00000100 10000100 00000100 00000000 00000000 00000000 00000000"
λ> bitShow $ pdep evens bs
"00000000 00000001 00010000 00010000 00100000 00000000 00000000 00000000"
```

I will leave it to the reader to work out why this works.  😉

# Next steps

We now have very fast rank-select operations for short bit-vectors of 64-bits,
which is sufficient for CSV streaming because it allows us to process 64 bytes of CSV
text at a time.

We also have the ability to split the odds and even bits out of our bit-string into
separate bit-strings.

All the conceptual pieces needed to produce the necessary rank-select bit-strings
for our high-performance RFC compliant CSV parser and pieces need to subsequently
traverse the CSV text and extract interesting data have been described.

In my next post I will talk about how SIMD instructions can be used to make
our parser go even faster!

Stay tuned!

[1]: ../posts/2018-08-08-data-parallel-rank-select-bit-string-construction.html
[2]: ../posts/2018-08-01-introduction-to-rank-select-bit-string.html
[3]: http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Bits.html#v:popCount
[4]: http://hackage.haskell.org/package/base
[5]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,4093&text=popcnt
[6]: https://github.com/ghc/ghc/blob/8df24474d0194d28b8273c1539af05793156e23f/libraries/base/Data/Bits.hs#L508
[7]: https://github.com/ghc/ghc/blob/ab55b4ddb717dab13d8b4900024ccbc8e9280c5c/compiler/nativeGen/X86/CodeGen.hs#L1861
[8]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#platform-specific-flags
[9]: http://delivery.acm.org/10.1145/3040000/3035963/p775-pandey.pdf?ip=144.132.158.200&id=3035963&acc=CHORUS&key=4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E6D218144511F3437&__acm__=1534943252_b1faffcd640ba8448fc9c811c1a3b2db
[10]: https://github.com/ghc/ghc/blob/44ba66527ae207ce2dd64eb2bce14656d474f6d1/compiler/nativeGen/X86/CodeGen.hs#L1990
[11]: https://ghc.haskell.org/trac/ghc/ticket/14206
[12]: https://phabricator.haskell.org/D4236
[13]: http://hackage.haskell.org/package/ghc-prim-0.5.2.0/docs/src/GHC.Prim.html#ctz64%23
[14]: https://github.com/haskell-works/bits-extra
