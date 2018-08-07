---
title: Implementing cut - a high performance data-parallel rank-select bit-string construction
author: John Ky
---

# Implementing cut - a high performance data-parallel rank-select bit-string construction

So far, I've posted about the problems with traditional parser and offered a
glimpse of how they might be solved, but succinct data structures are quite
alien and it isn't obvious the pay-off will be worthwhile.

In this post, I will motivate the use of rank-select bit-strings for CSV
by employing
[data-level parallelism](https://en.wikipedia.org/wiki/Data_parallelism)
techniques to demonstrate the performance benefits of succinct
data-structures based parsers over traditional parsers.

For simplicity, I will will be postponing the particular challenges of
implementing the particulars of the
[RFC standard for CSV](https://tools.ietf.org/html/rfc4180) for a later
time and instead work with a format where only the 

The simplified "CSV" grammar will only recognise delimiters and newlines
and have no support for escaping.  The format I describe is effectively
the kind of format that [`cut`](https://en.wikipedia.org/wiki/Cut_(Unix))
can process, so `cut` will serve as the performance baseline that I'm
aiming to improve upon.

## Data Parallelism

Data parallelism describes a computation that can be broken into smaller
computations that can be run in parallel.

Usually, a parsing is an inheritantly serial process due to the computation
consume the input text one character at a time, but because I've chosen
a very simple format where the identification of delimiters and newlines
are independent of what characters came before and what came after, I
have the option parse the text more than one character at a time.

Our parser would need to generate a `1` bit for every newline in one
of the rank-select bit-strings and a `1` bit for every newline and
delimiter in the other.

The example from the previous post was this:

```text
"name","age","profession"␤John,30,Code Monkey␤Kyle,40,Data Scrubber
0000001000001000000000000100001001000000000001000010010000000000000
0000000000000000000000000100000000000000000001000000000000000000000
```

This can be done with in one of two ways: broadword programming and
vectorisation instructions.

## Broadword progamming

Broadword programming, sometimes known as SWAR (SIMD Within A Register)
uses large registers (in our case 64-bit integer regsiters) as small
parallel computers that can process several pieces of information at a
time.

Let's first start with the newlines rank-select bit-string.

First we create a 64-bit word that contains 8 x 8-bit packed integers
each initialised to the ASCII code for the newline character.  For
example, the hexadecimal representation of the newline character is
`0x0a`, so the 64-bit word we are contructing will be:

```haskell
let wNewlines = 0x0101010101010101L * 0x0a = 0x0a0a0a0a0a0a0a0aL
```

The following is Little-Endian representation of 64-bit words.

```text
" n a m e " , "  a g e " , " p r  o f e s s i o n  " ␤ J o h n , 3 
226e616d65222c22 616765222c227072 6f66657373696f6e 220a4a6f686e2c33
0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a
28646b676f282628 6b6d6f2826287a78 656c6f7979636564 2800406562642639
0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 1 0 0 0 0 0 0 
                                                     ^^

0 , C o d e   M  o n k e y ␤ K y  l e , 4 0 , D a  t a   S c r u b 
302c436f6465204d 6f6e6b65790a4b79 6c652c34302c4461 7461205363727562
0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a 0a0a0a0a0a0a0a0a
3a2649656e6f2a47 6564616f73004173 666f263e3a264e6b 7e6b2a5969787f68
0 0 0 0 0 0 0 0  0 0 0 0 0 1 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 
                           ^^

b e r ␤
6265720a00000000
0a0a0a0a0a0a0a0a
686f78000a0a0a0a
0 0 0 1 0 0 0 0 
      ^^
```

We then take our input text, pad it with zero bytes until the next
64-bit boundary and cast the text into a 64-bit array.

We can then do our byte-by-byte comparisons 8-bytes at a time by
computing the XOR of every element against our `wNewlines` value.

The technique I will use is

can be used to build rank-select bit-string indexes for to implement a
higher performance version of `cut`.

I chose `cut` because the `cut` grammar has no concept of quoting and
so 

 unlocks
a technique called instruction parallelism, which allows us to process
multiple bytes in parallel

