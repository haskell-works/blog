---
title: Data-Parallel Rank-Select Bit-String construction
author: John Ky
---

So far, I've [posted](../posts/2018-08-01-introduction-to-rank-select-bit-string.html)
about the problems with traditional parser and offered a
glimpse of how they might be solved, but succinct data structures are quite
alien and it isn't obvious the pay-off will be worthwhile.

In this post, I demonstrate how use broadword programming techniques to
exploit [data-level parallelism](https://en.wikipedia.org/wiki/Data_parallelism)
in the parsing of a CSV-like format.

Doing so allows us to parse the text 8-bytes at a time instead of one
byte at a time as a traditional parser would.

For simplicity, I will be postponing the particular challenges of
implementing the particulars of the
[RFC standard for CSV](https://tools.ietf.org/html/rfc4180) for a later
time and instead work with a simplified format.

The simplified "CSV" grammar will only recognise delimiters and newlines
and have no support for escaping.

The format I describe is effectively
the kind of format that the [`cut`](https://en.wikipedia.org/wiki/Cut_(Unix))
command line tool can process, so `cut` will at some point serve as the
performance baseline that I'm aiming to improve upon.

## Data Parallelism

Data parallelism describes a computation that can be decomposed into smaller
computations that can be run in parallel.

We usually think of parsing is an inheritantly serial process due to the fact
the parser must consume the input text one character at a time.

But because I've chosen a simplified format where the identification of
delimiters and newlines are independent of what characters came before
and what came after, there ought to be a way to parse the text more than
one character at a time for the purposes of building a rank-select
bit-string.

Our parser would need to generate a `1` bit for every newline in one
of the rank-select bit-strings and a `1` bit for every newline and
delimiter in the other.

Let's use the example from the previous post to demonstrate how this
can be done:

```text
"name","age","profession"␤John,30,Code Monkey␤Kyle,40,Data Scrubber
0000001000001000000000000100001001000000000001000010010000000000000
0000000000000000000000000100000000000000000001000000000000000000000
```

The first line is the text we want to use to construct the following
two bit-strings.

Note that `␤` represenes a newline ASCII character

## Broadword progamming

Broadword programming, sometimes known as SWAR (SIMD Within A Register)
uses large registers (in our case 64-bit integer registers) as small
parallel computers that can process several pieces of information at a
time.

We will be using this technique to parse our text 8-bytes at a time.

Before we start we will need the following operations:

```haskell
(.&.) :: Word64 -> Word64 -> Word64 -- Compute the bit-wise AND of two integers
(.|.) :: Word64 -> Word64 -> Word64 -- Compute the bit-wise OR of two integers
(.^.) :: Word64 -> Word64 -> Word64 -- Compute the bit-wise XOR of two integers
comp  :: Word64 -> Word64           -- Compute the bit-wise complement of an integer
(.>.) :: Word64 -> Count -> Word64  -- Compute the right shift of an integer by the given offset in Big-Endian
                                    -- Note, this is equivalent to a left shift of an integer in Little-Endian
```

These operators are pretty standard, but pay close attention to the shift
operator, which is the standard left shift operator.

When the bits are laid out in Little-Endian order the right shift operator will
actually shift all the bits *to the LEFT*.

In all of the diagrams, bits and bytes will be laid out in Little-Endian
because it is the natural layout for succinct data structures but it can
be very confusing so bear with me and keep this in mind.

Our first task is to build the following rank-select bit-string from
our text.

```text
"name","age","profession"␤John,30,Code Monkey␤Kyle,40,Data Scrubber
0000000000000000000000000100000000000000000001000000000000000000000
```

To do this, we first create a 64-bit word that contains 8 x 8-bit packed
integers each initialised to the ASCII code for the newline character.  For
example, the hexadecimal representation of the newline character is
`0x0a`, so the 64-bit word we are contructing will be:

```haskell
let wNewlines = 0x0101010101010101L * 0x0a = 0x0a0a0a0a0a0a0a0aL
```

The next step is to take the text and pad it with zero bytes to the nearest
64-bit boundary.  Then take the entire text and cast it to a vector of
Little-Endian 64-bit integers.

This leads to a view of memory shown below indicated by `text`.

```text
             Bytes packed into Little-Endian Word64 integers
           ┌─────────────────────────┼─────────────────────────┐
┌──────────┴──────────┐   ┌──────────┴──────────┐   ┌──────────┴──────────┐
"  n  a  m  e  "  ,  "    a  g  e  "  ,  "  p  r    o  f  e  s  s  i  o  n   
22 6e 61 6d 65 22 2c 22   61 67 65 22 2c 22 70 72   6f 66 65 73 73 69 6f 6e   text ─────────┐
0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   mask ────────┐│
28 64 6b 67 6f 28 26 28   6b 6d 6f 28 26 28 7a 78   65 6c 6f 79 79 63 65 64   comparison <┬┴┘ (.^.)
0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0    newlines <──┘ cmpzero
                                                                            
                                                                            
  ┌──┐                                                            ┌──┐                        
" │$ │J  o  h  n  ,  3    0  ,  C  o  d  e     M    o  n  k  e  y │␤ │K  y 
22│0a│4a 6f 68 6e 2c 33   30 2c 43 6f 64 65 20 4d   6f 6e 6b 65 79│0a│4b 79   text ─────────┐
0a│0a│0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a│0a│0a 0a   mask ────────┐│
28│00│40 65 62 64 26 39   3a 26 49 65 6e 6f 2a 47   65 64 61 6f 73│00│41 73   comparison <┬┴┘ (.^.)
0 │1 │0  0  0  0  0  0    0  0  0  0  0  0  0  0    0  0  0  0  0 │1 │0  0    newlines <──┘ cmpzero
  └┬─┘                                                            └┬─┘  
   └──────────────────────────────────────────────┬────────────────┘
                                                  │                            
                                                  │         ┌──┐               
l  e  ,  4  0  ,  D  a    t  a     S  c  r  u  b  │ b  e  r │␤ ├───────────┐   
6c 65 2c 34 30 2c 44 61   74 61 20 53 63 72 75 62 │ 62 65 72│0a│00 00 00 00│  text ─────────┐
0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a │ 0a 0a 0a│0a│0a 0a 0a 0a│  mask ────────┐│
66 6f 26 3e 3a 26 4e 6b   7e 6b 2a 59 69 78 7f 68 │ 68 6f 78│00│0a 0a 0a 0a│  comparison <┬┴┘ (.^.)
0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0  │ 0  0  0 │1 │0  0  0  0 │  newlines <──┘ cmpzero
                                                  │         └┬─┴───┬───────┘  
            Bits corresponding to newlines that   │          │     └─Padding
            need to be set to 1 ──────────────────┴──────────┘
```

Then for each 64-bit value in the vector take the exclusive or `(.^.)` with
the `wNewlines` mask.  The resulting vector of 64-bit vector when cast back
to a vector of bytes will then contain a zero value for every character
in the original text that was a newline and a non-zero value otherwise.

This is very close to what we want.  All we need to do now is to collapse
every non-zero byte into a `0` bit and every zero byte to a `1` bit.

The problem we are trying to solve can be exemplified by the following
example where we need a function `cmpzero` that condenses the bytes in
the `comparison` row to the bits in the `newlines` row:

```text
              ┌──┐
65 64 61 6f 73│00│41 73   comparison ─┐
0  0  0  0  0 │1 │0  0    newlines <──┘
              └──┘
```

This can be done in a series of bit-wise manipulations described below:

```text
T   text ┌─── 10100110 00100110 10000110 11110110 11001110 00000000 10000010 11001110
         │                                                                           
    mask │┌── 00001111 00001111 00001111 00001111 00001111 00001111 00001111 00001111
A   (.&.)├┴─> 00000110 00000110 00000110 00000110 00001110 00000000 00000010 00001110 ─┐    
B        │  ┌ 01100000 01100000 01100000 01100000 11100000 00000000 00100000 11100000 <┘ (.>. 4)
         │  └────────────────────────────────────────────────────────────────────────────┐
    mask │┌── 11110000 11110000 11110000 11110000 11110000 11110000 11110000 11110000    │
C   (.&.)└┴─> 10100000 00100000 10000000 11110000 11000000 00000000 10000000 11000000 ──┐│
                                                                                        ││
D        ┌─── 11100000 01100000 11100000 11110000 11100000 00000000 10100000 11100000 <─┴┘(.|.)
         │                                                                             
    mask │┌── 00110000 00110000 00110000 00110000 00110000 00110000 00110000 00110000 mask 
E   (.&.)├┴─> 00100000 00100000 00100000 00110000 00100000 00000000 00100000 00100000 ─┐    
F        │  ┌ 10000000 10000000 10000000 11000000 10000000 00000000 10000000 10000000 <┘ (.>. 2)
         │  └────────────────────────────────────────────────────────────────────────────┐
G   mask │┌── 11000000 11000000 11000000 11000000 11000000 11000000 11000000 11000000    │
H   (.&.)└┴─> 11000000 01000000 11000000 11000000 11000000 00000000 10000000 11000000 ──┐│
                                                                                        ││
I        ┌─── 11000000 11000000 11000000 11000000 11000000 00000000 10000000 11000000 <─┴┘(.|.)
         │                                                                            
    mask │┌── 01000000 01000000 01000000 01000000 01000000 00000000 01000000 01000000 
J   (.&.)├┴─> 01000000 01000000 01000000 01000000 01000000 00000000 01000000 01000000 ─┐    
K        │  ┌ 10000000 10000000 10000000 10000000 10000000 00000000 10000000 10000000 <┘ (.>. 1)
         │  └────────────────────────────────────────────────────────────────────────────┐
    mask │┌── 10000000 10000000 10000000 10000000 10000000 00000000 10000000 10000000    │
L   (.&.)└┴─> 10000000 10000000 10000000 10000000 10000000 00000000 10000000 10000000 ──┐│
                                                                                        ││
M             10000000 10000000 10000000 10000000 10000000 00000000 10000000 10000000 <─┴┘(.|.)
              │        │        │        └───┐┌───┘        │        │        │
              │        │        └───────────┐││┌───────────┘        │        │
              │        └───────────────────┐││││┌───────────────────┘        │
              └───────────────────────────┐││││││┌───────────────────────────┘
                                          11111011─┐pext
                                          00000100<┘comp
```

We start with the following computations:

* At `T` we have our 64-bit integer that contains 8-bytes of our text
* At `A` we mask out first half of every byte in our word `T`.
* At `B` we shift the second half of every byte into the first half position of every byte in our word.
* At `C` we mask out the second half of every byte in our word `T`.
* At `D` we compute `A .|. C`

At this point we have compressed result into the first half of every byte.  We repeat this to
compress further to the first quarter of our bytes similarly:

* At `E` we mask out first quarter of every byte in our word `D`.
* At `F` we shift the second quarter of every byte into the first quarter of every byte in our word.
* At `H` we mask out the second quarter of every byte in our word `D`.
* At `I` we compute `F .|. H`

Finally we do this one more time to compress the first quarter of every byte in our word to the first
bit of every byte in our word.

* At `J` we mask out first bit of every byte in our word `H`.
* At `K` we shift the second bit of every byte into the first bit of every byte in our word.
* At `L` we mask out the second bit of every byte in our word `H`.
* At `M` we compute `J .|. L`

Now we are two steps away from the 8-bits of rank-select bit-string we need for our 8 input characters.

We need some way to extract the first bit of every byte in our word, an operation I will call `pext` or
parallel extract, and then take the complement of the result to get the rank-select bit-string we
sought.

The other rank-select bit-string which marks delimiters and newlines can be derived by applying the
same algorithm for the delimiters and the newlines separately then taking the bitwise OR `(.|.)` of
the two resulting bit-strings.

The resulting code is show below.

```haskell
testWord8s :: Word64 -> Word64
testWord8s w =  let w8s = w
                    w4s = (w8s .&. 0x0f0f0f0f0f0f0f0f) .|. (w8s .&. 0xf0f0f0f0f0f0f0f0 .>. 4)
                    w2s = (w4s .&. 0x0707070707070707) .|. (w4s .&. 0x7070707070707070 .>. 2)
                    w1s = (w2s .&. 0x0303030303030303) .|. (w2s .&. 0x3030303030303030 .>. 1)
                in  pext w1s 0x0101010101010101
{-# INLINE testWord8s #-}
```

All up we've used the following operations:

* `(.&.)` x 6
* `(.|.)` x 3
* `(.>.)` x 3
* `pext` x 1
* `load` x 1
* `store` x 1

Which adds up to `13` very fast register only instructions plus `2` implied memory instructions.

# Optimising our bit-string construction code

If you look at the `M` row in the diagram you will notice that near the end of the computation
we only use the least significant bit from each of the 8 bytes in our word and don't actually
care what the values of the other bits are.

We can exploit this fact to remove some of the operations from our computation by marking
those bits as don't care or `x`, and tracing those bits backwards through the computation
and then figuring out which operations we can omit.

```text
T   text ┌─── 10100110 00100110 10000110 11110110 11001110 00000000 10000010 11001110
         │                                                                           
A   noop ├──> xxxx0110 xxxx0110 xxxx0110 xxxx0110 xxxx1110 xxxx0000 xxxx0010 xxxx1110 ─┐    
B        │  ┌ 0110xxxx 0110xxxx 0110xxxx 0110xxxx 1110xxxx 0000xxxx 0010xxxx 1110xxxx <┘ (.>. 4)
         │  └────────────────────────────────────────────────────────────────────────────┐
C   noop └──> 1010xxxx 0010xxxx 1000xxxx 1111xxxx 1100xxxx 0000xxxx 1000xxxx 1100xxxx ──┐│
                                                                                        ││
D        ┌─── 1110xxxx 0110xxxx 1110xxxx 1111xxxx 1110xxxx 0000xxxx 1010xxxx 1110xxxx <─┴┘(.|.)
         │                                                                             
E   noop ├──> xx10xxxx xx10xxxx xx10xxxx xx11xxxx xx10xxxx xx00xxxx xx10xxxx xx10xxxx ─┐    
F        │  ┌ 10xxxxxx 10xxxxxx 10xxxxxx 11xxxxxx 10xxxxxx 00xxxxxx 10xxxxxx 10xxxxxx <┘ (.>. 2)
         │  └────────────────────────────────────────────────────────────────────────────┐
G   noop └──> 11xxxxxx 01xxxxxx 11xxxxxx 11xxxxxx 11xxxxxx 00xxxxxx 10xxxxxx 11xxxxxx ──┐│
                                                                                        ││
I        ┌─── 11xxxxxx 11xxxxxx 11xxxxxx 11xxxxxx 11xxxxxx 00xxxxxx 10xxxxxx 11xxxxxx <─┴┘(.|.)
         │                                                                            
J   noop ├──> x1xxxxxx x1xxxxxx x1xxxxxx x1xxxxxx x1xxxxxx x0xxxxxx x1xxxxxx 01xxxxxx ─┐    
K        │  ┌ 1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx x0xxxxxx 1xxxxxxx 1xxxxxxx <┘ (.>. 1)
         │  └────────────────────────────────────────────────────────────────────────────┐
L   noop └──> 1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx 1xxxxxxx 1xxxxxxx ──┐│
                                                                                        ││
M             1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx 1xxxxxxx 1xxxxxxx <─┴┘(.|.)
              │        │        │        └───┐┌───┘        │        │        │
              │        │        └───────────┐││┌───────────┘        │        │
              │        └───────────────────┐││││┌───────────────────┘        │
              └───────────────────────────┐││││││┌───────────────────────────┘
                                          11111011─┐pext
                                          00000100<┘comp
```

As a result of tracing the don't care or `x` bits backwards through the computation,
I was able to remove six masks and six AND `(.&.)` operations.

I left the rows `A`, `C`, `E`, `G`, `J`, and `L` in as non-operations (`noop`) because
I wanted to annotate the additional bits I don't care beyond those I already don't care
about in the original value.

The resulting code is much simpler and faster:

```haskell
testWord8s :: Word64 -> Word64
testWord8s w =  let w8s = w
                    w4s = w8s .|. (w8s .>. 4)
                    w2s = w4s .|. (w4s .>. 2)
                    w1s = w2s .|. (w2s .>. 1)
                in  pext w1s 0x0101010101010101
{-# INLINE testWord8s #-}
```

The optimisations meas we now use only these operations:

* `(.|.)` x 3
* `(.>.)` x 3
* `pext` x 1
* `load` x 1
* `store` x 1

This adds up to `7` very fast register only instructions plus `2` implied memory instructions,
which is very close to `1` instruction per byte.

## Unanswered questions

What's nice about it is that we are parsing 8-bytes at a time using fewer instructions and we've
managed to avoid any branches or memory allocations within each iteration that could slow
the iteration down.

Nevertheless, some benchmarks will be necessary to compare this approach to one that parses a byte at
a time.

You may have noticed that I have used the `pext` operation without properly describing how it
works nor explained why that operation should be fast.

I will follow up in a future post to explain the `pext` operation in detail and offer some benchmarks
to show the degree of speed up we might expect from exploiting data parallelism in our parsers.

## Source code
You can play with the source code here:

https://github.com/haskell-works/hw-simd/blob/master/src/HaskellWorks/Data/Simd/Internal/Bits.hs#L9
