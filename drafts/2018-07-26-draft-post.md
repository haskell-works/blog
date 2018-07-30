---
title: Introduction to the succinct semi-index
author: John Ky
---

[Last week](../posts/2018-07-25-problem-of-parsing-large-datasets.html) we
looked at traditional whole-document parsers struggle to parse
big files.

<img style="float: right; height: 300px; width: 300px;" src="/images/golden-toilet.png">
Such parsers both use much memory and are too slow, being orders of
magnitude slower than what IO bandwidth would allow.

In some sense, we can understand the slowness as a consequence large memory usage:
All that memory access does not come for free time-wise.

Traditional whole-document parsers spend a lot of time allocating memory, assigning
pointers, following indirections and touching new memory that isn't cached
in the CPU cache where it could have been accessed much more quickly.

To achieve high performance, the parser needs to do as little possible, but
traditional parsers are actually creating a lot of work for the hardware,
that is incidental to solving the problem of making the document data accessible
and much of it is hidden from us, the developer, by language and hardware
abstractions, so the overhead is easy to miss.

We have seen how objects are severely expensive, especially when allocated
en-mass.  For our use-case their cost disproportionately exceeds their utility.

# If not objects then what

If you want to minimise memory usage, the first thing you should do is avoid
duplicating data.

All the data we want to access is already in the document.  Copying that data
into intermediate objects so that we can work with them is wasteful.

<img style="float: right; height: 300px; width: 300px;" src="/images/bits-in-perspective.jpg">
Instead we want to reuse the data in its original form as much as possible
and leave the parsing of small individual elements of the document to
the last moment just before we use them and in that way avoid parsing all
the parts of the document we don't need.

What prevents us from doing that exactly that is that we lack the means
to navigate the structure of the document to locate the various pieces
of data in the document we are interested in.

This structure is exactly what our object-based document object model
provided, but to avoid paying their dues, we must find another way.

But if not objects, then what?

If you want to find something extremely small and light weight, you could do
worse than choose the humble bit.  It is 64 times smaller than a pointer
and many multiples of that smaller than an object.

So let's do exactly that: Let's use bits!

# The rank-select bit string

Before we can pull this off we are going to have to learn us some concepts.

Imagine a string of bits.  Not unlike the following:

```haskell
let bs = "101000000010000000100000010001000000100000"
```

In order to query this bit string we will be using two very powerful
query operations **rank** and **select**.

The pseudocode for these to operations are provided below in Haskell which
you can drop into your Haskell repl to observe the behaviour of these two
operations:

```haskell
import Data.List

popCount1 :: String -> Int
popCount1 bs = length (filter (== '1') bs)

rank1 :: String -> Int -> Int
rank1 bs n = popCount1 (take n bs)

select1 :: String -> Int -> Int
select1 bs n = length (head (dropWhile ((< n) . popCount1) (inits (filter isBinary bs))))
  where isBinary c = c == '0' || c == '1'
```

The first function `popCount1` is the **population** operation (sometimes called the
[hamming weight](https://en.wikipedia.org/wiki/Hamming_weight)).
It tells us how many `1` bits there are in our bit string.

```haskell
λ> popCount1 bs
7
```

The second function `rank1` is the **rank** operation which tells us the population count of
the prefix of our substring of the given length `n`.

Here are some example **rank** queries:

```haskell
λ> rank1 bs 0
0
λ> rank1 bs 1
1
λ> rank1 bs 2
1
λ> rank1 bs 3
2
λ> rank1 bs 14
3
```

The third function `select1` is the **select** operation which tells us smallest prefix of the given
bit string that has the given population `n`.

Here are some example **select** queries:

```haskell
λ> select1 bs 0
0
λ> select1 bs 1
1
λ> select1 bs 2
3
λ> select1 bs 3
11
λ> select1 bs 4
19
```

In less precise terms, the **rank** gives us how many `1s` up to a given position `n`
in our bit string and and **select** gives us the position of the `nᵗʰ` `1` in our
bit string. どうしても

# Rank Select Bit String as a JSON Semi-index

We will now use the rank-select bit-string as a semi-index, which is to say we will
use it to locate interesting locations in our JSON document.

In our semi-index, every bit in the rank-select bit-string corresponds to a byte in
the original document of the same position.  The value of each bit is chosen such
that when the byte is the start of an interesting part of the document structurally
it will be set to `1`, or `0` otherwise.

For JSON, the beginning of every object (indicated by `{`), every array (indiciated by
`[`), every field or value will be marked with a `1`.

```json
{ "name": "John", "age": 30, "car": null, colors: [1, 2, 3] }
1010000000100000001000000100010000001000001000000011001001000
```

What this gives us is the ability to locate the nᵗʰ structurally important location
in the document with **rank-select** operations.

For example the 6th structurally important location marks the beginning of the
field-name `"car"`:

```haskell
λ> let text = "{ \"name\": \"John\", \"age\": 30, \"car\": null, numbers: [1, 2, 3] }"
λ> let bs   = "101 00000 001 00000 0010 00000 10001000 00010 000010000000011001001000"
λ> let offset = select1 bs 6
31
λ> drop (offset - 1) text
"\"car\": null, numbers: [1, 2, 3] }"
```

In another example, the 9th structurally important location marks the
beginning of the JSON array:

```haskell
λ> let text = "{ \"name\": \"John\", \"age\": 30, \"car\": null, numbers: [1, 2, 3] }"
λ> let bs   = "101 00000 001 00000 0010 00000 10001000 00010 000010000000011001001000"
λ> let offset = select1 bs 9
52
λ> drop (offset - 1) text
"[1, 2, 3] }"
```

It's also worth noting that each successive `1` bit in the rank-select bit-string
identifies nodes of the document according to pre-order traversal and that the
rank-select bit-string in combination with the original text can be used to
identify the type of the node in O(1) time, by testing the character pointed to
by the rank-select bit-string.

For example, I know the 6ᵗʰ node in the document pre-order traversal is a
string because the character at line `31` is a `"`, whilst the 9ᵗʰ node by
pre-order traversal is an array because the character at line `52` is a `[`.]

# Rank Select Bit String as a CSV Semi-index

Like-wise rank-select bit-strings can be as semi-index into a CSV document.

Take the following CSV document

```text
"name","age","profession"
John,30,Code Monkey
Kyle,40,Data Scrubber
```

I will represent this document on a single line for easier comparison with
the rank the rank-select bit-strings:

```text
"name","age","profession"␤John,30,Code Monkey␤Kyle,40,Data Scrubber
0000001000001000000000000100001001000000000001000010010000000000000
0000000000000000000000000100000000000000000001000000000000000000000
```

In this case, I've chosen to use two rank-select bit-strings, one to
to mark both delimiters and newlines and the other to mark newlines
only.

What this gives us is the ability to locate the nᵗʰ structurally important location
in the document with **rank-select** operations.

For example the 6th structurally important location marks the beginning of the
field-name `"car"`:

```haskell
λ> let text = "{ \"name\": \"John\", \"age\": 30, \"car\": null, numbers: [1, 2, 3] }"
λ> let bs   = "101 00000 001 00000 0010 00000 10001000 00010 000010000000011001001000"
λ> let offset = select1 bs 6
31
λ> drop (offset - 1) text
"\"car\": null, numbers: [1, 2, 3] }"
```

And the 9ᵗʰ structurally important location marks the beginning of the
JSON array:

```haskell
λ> let text = "{ \"name\": \"John\", \"age\": 30, \"car\": null, numbers: [1, 2, 3] }"
λ> let bs   = "101 00000 001 00000 0010 00000 10001000 00010 000010000000011001001000"
λ> let offset = select1 bs 9
52
λ> drop (offset - 1) text
"[1, 2, 3] }"
```
