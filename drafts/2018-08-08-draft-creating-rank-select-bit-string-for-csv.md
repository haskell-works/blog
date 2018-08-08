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
             Bytes packed into Little-Endian Word64 integers
           ┌─────────────────────────┼─────────────────────────┐
┌──────────┴──────────┐   ┌──────────┴──────────┐   ┌──────────┴──────────┐
"  n  a  m  e  "  ,  "    a  g  e  "  ,  "  p  r    o  f  e  s  s  i  o  n   
22 6e 61 6d 65 22 2c 22   61 67 65 22 2c 22 70 72   6f 66 65 73 73 69 6f 6e   text ─────────┐
0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   mask ────────┐│
28 64 6b 67 6f 28 26 28   6b 6d 6f 28 26 28 7a 78   65 6c 6f 79 79 63 65 64   comparison <┬┴┘ xor
0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0    newlines <──┘ cmpzero
                                                                            
                                                                            
  ┌──┐                                                            ┌──┐                        
" │$ │J  o  h  n  ,  3    0  ,  C  o  d  e     M    o  n  k  e  y │␤ │K  y 
22│0a│4a 6f 68 6e 2c 33   30 2c 43 6f 64 65 20 4d   6f 6e 6b 65 79│0a│4b 79   text ─────────┐
0a│0a│0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a│0a│0a 0a   mask ────────┐│
28│00│40 65 62 64 26 39   3a 26 49 65 6e 6f 2a 47   65 64 61 6f 73│00│41 73   comparison <┬┴┘ xor
0 │1 │0  0  0  0  0  0    0  0  0  0  0  0  0  0    0  0  0  0  0 │1 │0  0    newlines <──┘ cmpzero
  └┬─┘                                                            └┬─┘  
   └──────────────────────────────────────────────┬────────────────┘
                                                  │                            
                                                  │         ┌──┐               
l  e  ,  4  0  ,  D  a    t  a     S  c  r  u  b  │ b  e  r │␤ ├───────────┐   
6c 65 2c 34 30 2c 44 61   74 61 20 53 63 72 75 62 │ 62 65 72│0a│00 00 00 00│  text ─────────┐
0a 0a 0a 0a 0a 0a 0a 0a   0a 0a 0a 0a 0a 0a 0a 0a │ 0a 0a 0a│0a│0a 0a 0a 0a│  mask ────────┐│
66 6f 26 3e 3a 26 4e 6b   7e 6b 2a 59 69 78 7f 68 │ 68 6f 78│00│0a 0a 0a 0a│  comparison <┬┴┘ xor
0  0  0  0  0  0  0  0    0  0  0  0  0  0  0  0  │ 0  0  0 │1 │0  0  0  0 │  newlines <──┘ cmpzero
                                                  │         └┬─┴───┬───────┘  
            Bits corresponding to newlines that   │          │     └─Padding
            need to be set to 1 ──────────────────┴──────────┘
```

```
              ┌──┐
65 64 61 6f 73│00│41 73   comparison ─┐
0  0  0  0  0 │1 │0  0    newlines <──┘
              └──┘
```

```text

01100101 01100100 01100001 01101111 01110011 00000000 01000001 01110011 text ──┐
                                                                               │
11110000 11110000 11110000 11110000 11110000 11110000 11110000 11110000 mask ─┐│
01100000 01100000 01100000 01100000 01110000 00000000 01000000 01110000 <───┬─┴┤(.&.)
00000110 00000110 00000110 00000110 00000111 00000000 00000100 00000111 <┬──┘  │(.>. 4)
                                                                         └─────│┐
00001111 00001111 00001111 00001111 00001111 00001111 00001111 00001111 mask ─┐││
00000101 00000100 00000001 00001111 00000011 00000000 00000001 00000011 <──┬──┴┘│(.&.)
                                                                           │    │
00000111 00000110 00000111 00001111 00000111 00000000 00000101 00000111 <┬─┴────┘(.|.)
                                                                         └─────┐
00001100 00001100 00001100 00001100 00001100 00001100 00001100 00001100 mask ─┐│
00000100 00000100 00000100 00001100 00000100 00000000 00000100 00000100 <───┬─┴┤(.&.)
00000001 00000001 00000001 00000011 00000001 00000000 00000001 00000001 <─┬─┘  │(.>. 2)
                                                                          └────│┐
00000011 00000011 00000011 00000011 00000011 00000011 00000011 00000011 mask ─┐││
00000011 00000010 00000011 00000011 00000011 00000000 00000001 00000011 <──┬──┴┘│(.&.)
                                                                           │    │
00000011 00000011 00000011 00000011 00000011 00000000 00000001 00000011 <┬─┴────┘(.|.)
                                                                         └─────┐ 
00000010 00000010 00000010 00000010 00000010 00000000 00000010 00000010 mask ─┐│
00000010 00000010 00000010 00000010 00000010 00000000 00000010 00000010 <───┬─┴┤(.&.)
00000001 00000001 00000001 00000001 00000001 00000000 00000001 00000001 <─┬─┘  │(.>. 1)
                                                                          └────│┐
00000001 00000001 00000001 00000001 00000001 00000000 00000001 00000001 mask ─┐││
00000001 00000001 00000001 00000001 00000001 00000000 00000001 00000001 <┬────┴┘│(.&.)
                                                                         └─────┐│
00000001 00000001 00000001 00000001 00000001 00000000 00000001 00000001 <──────┴┘(.|.)
       │        │        │        └┐┌──────┘        │        │        │
       │        │        └────────┐││┌──────────────┘        │        │
       │        └────────────────┐││││┌──────────────────────┘        │
       └────────────────────────┐││││││┌──────────────────────────────┘
                                11111011
```


We then take our input text, pad it with zero bytes until the next
64-bit boundary and cast the text into a 64-bit array.

We can then do our byte-by-byte comparisons 8-bytes at a time by
computing the XOR of every element against our `wNewlines` value.

