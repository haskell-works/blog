---
title: Introduction to the succinct semi-index
author: John Ky
---

[Last week](../posts/2018-07-25-problem-of-parsing-large-datasets.html) we
looked at traditional whole-document parsers struggle to parse
big files.

<img style="float: right; height: 300px; width: 300px;" src="/images/golden-toilet.png">
Such parsers both take use much memory and are too slow, being orders of
magnitude slower than what IO bandwidth would allow.

In some sense, we can understand the slowness as a consequence large memory usage.
All that memory access does not come for free time-wise.

Traditional whole-document parsers spend a lot of time allocating memory, assigning
pointers, following indirections and touching new memory that isn't cached
in the CPU cache where it could have been accessed much more quickly.

To achieve high performance, the parser needs to do as little possible, but
traditional parsers are actually creating a lot of work for the hardware,
that is incidental to solving the problem of making the document data accessible
and much of it is hidden from us, the developer, by language and hardware
abstractions, so the overhead is easy to miss.

We have seen how the over-reliance on objects, and whilst they do the job,
are severely expensive, especially when allocated en-mass.  For our use-case
their cost disproportionately exceeds their utility.

# If not objects then what

If you want to minimise memory usage, the first thing you should do is avoid
duplicating data.

All the data we want to access is already in the document.

Copying that data into intermediate objects so that we can work with them
is wasteful.

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
rank1 bs n = popCount (take n bs)

select1 :: String -> Int -> Int
select1 bs n = length (head (dropWhile ((< n) . popCount) (inits bs)))
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
in our bit string and and **select** gives us the position of the `n`th `1` in our
bit string.

```json
{ "name": "John", "age": 30, "car": null }
```
