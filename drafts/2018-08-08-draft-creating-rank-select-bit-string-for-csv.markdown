---
title: Parsing with Data-level Parallelism 
author: John Ky
---

# Instruction Parallelism

So far, I've post about the problems with traditional parser and offered a
glimpse of how they might be solved, but succinct data structures are quite
alien and it isn't obvious the pay off will be worthwhile.

In this post, I will motivate the use of rank-select bit-strings, but
showing how
[data-level parallelism](https://en.wikipedia.org/wiki/Data_parallelism)
can be used to build rank-select bit-string indexes for a simplified CSV
grammar.

 unlocks
a technique called instruction parallelism, which allows us to process
multiple bytes in parallel

