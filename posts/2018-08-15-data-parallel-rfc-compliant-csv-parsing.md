---
title: RFC compliant data-parallel CSV parsing
author: John Ky
---

In last week's [post](../posts/2018-08-08-data-parallel-rank-select-bit-string-construction.html)
I described how to exploit data-parallelism to
build a rank-select bit-string for a `cut` compatible
delimeter-separated-values format, parsing 8-bytes at-a-time.

In this post we will look at how to do the same for the CSV format
described in [RFC4180](https://tools.ietf.org/html/rfc4180),
where complicating factors such as quotes, escaping, and quoted
control characters cannot be ignored.

## The RFC format

The data-parallel parser will need to deal with the following cases
where carriage-return characters are represented by `␍` and line-feed
characters are represented by ␊:

When no quotations are used:

```text
aaa,bbb,ccc␍␊
zzz,yyy,xxx
```

When quotations are used only around the entire fields containing no newline characters:

```text
"aaa","bbb","ccc"␍␊zzz,yyy,xxx
```

When quotations are used only around the entire fields which may contain newline or delimiter
characters:

```text
"aaa","b␍␊
b,b","ccc"␍␊
zzz,yyy,xxx
```

When quotations are used within quoted fields represented by two consecutive double-quotes:

```text
"aaa","b""bb","ccc"
```

## The Parser Specification

Because we are using rank-select bit-strings to index into the original text of the
document and to minimise work, the parser is not expected to do additional processing
to remove surrounding double quotes, escape quotes nor remove control characters.

These are instead left to a higher level parser built on top of our parser.

This means any strings the parser will yield will be a strict substring of the
original document.

The parser will parse the earlier examples to produce results as described below.

When no quotations are used the substring representing the field text is returned in the result.

The line-feed byte will act as our newline character.  Control chracters other than the
row-delimiting line-feed byte are also included in the result with their closest field
as is:

```haskell
[ ["aaa", "bbb", "ccc␍"]
, ["zzz", "yyy", "xxx"]
]
```

When quotations are used only around the entire fields containing no newline characters,
the field is returned exactly as they appear in the original document including the
surrounding double-quotes:

```haskell
[ ["\"aaa\"", "\"bbb\"","\"ccc\"␍"]
, ["zzz", "yyy", "xxx"]
]
```

When quotations are used only around the entire fields containing line-feed characters
or delimiters, such characters will be returned as part of their field in the result:

```haskell
[ ["\"aaa\"", "\"b␍␊b,b\"", "\"ccc\"␍"]
, ["zzz", "yyy", "xxx"]
]
```

When quotations are used within quoted fields represented by two consecutive double-quotes,
the consecutive double-quotes will be returned in the results as is:

```haskell
[ ["\"aaa\"", "\"b\"\"bb\"", "\"ccc\""]
]
```

Returning substrings of the raw text as is in this way is good for performance because it
means that in situtations where user-code only wishes to access some fields of each row in
the document, they do not need to incur the costs of properly parsing the fields they don't
need.

This is also beneficial for cases where the user wishes to byte-copy fields from the input
document into an output document, for example running a program to select desired fields
from a CSV document into a new CSV document.

## Deriving the strategy

In order to demonstrate the strategy, I will combine all the possible cases into a single
document:

```text
aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
```

We will build the rank-select bit-strings for newlines and delimiters exactly as we did
for the non-conformant parser in the last post:

```text
text:     aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
markers:  00010001000010000000100010001001000
newlines: 00000000000010000000000010000000000
```

Unfortunately, these rank-select bit-strings, are incorrect because they do not properly
handle the case where line-feed or delimiter characters are embedded in a quoted field.

I've marked the incorrectly set bits with an asterisk `*`:

```text
text:     aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
markers:  000100010000000000001000*000100*000
newlines: 000000000000000000000000*0000000000
```

We will need to somehow clear the `*` bits in our rank-select bit-strings.

Since we desire to use a data-parallel approach to parsing CSV, we can start by building
a rank-select bit-string for the double-quotes in our CSV document, just as we did for
delimeters a new newlines.


```text
text:     aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
markers:  000100010000000000001000*000100*000
newlines: 000000000000000000000000*0000000000
quotes:   00000000000001011001010000010100001
```

But this doesn't actually help me.

I want to clear all the bits marked by `*` without clearing the the other bits.

I can't use `quotes` as a mask because everywhere there
is a `1` bit regardless of whether it is correct (ie. `1`) or incorrect (ie. `*`), the
corresponding bit in `quotes` is zero.

Furthermore I don't actually care about the `1` bits in `quotes` because there can
never be a delimiter or newline at a position that is already occupied by a quote `"`.

Instead I need something like `mask` below, which has a `1` bit everywhere that is
unquoted and a `0` bit everywhere that is quoted:

```text
text:     aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
markers:  000100010000000000001000*000100*000
newlines: 000000000000000000000000*0000000000
quotes:   00000000000001011001010000010100001
mask:     1111111111111?0??00?1?00000?1?0000?
```

I've also marked the locations that correspond to a double-quote as `?` because I
don't care about the bits at these locations because they can't affect the outcome
when I use `mask` to mask out the undersireable bits in the rank-select bit-string.

So how do I build a bit-string like `mask`?

The information I need to build this bit-string is contained withing the `quotes`
bit-string.  If I traverse the `quotes` bit-string bit-by-bit with the initial state
of `1` and I flip my state every-time I encounter a `1` in `quotes`, I get the
bit-string I need.

But this approach undermines the performance of the parser because now we
are reduced to parsing the bit-string bit-by-bit, which is just as bad as
parsing the CSV text byte-by-byte.

We have failed to exploit data parallelism to give us the performance we want.

But all is not lost.

Let's look at our `quotes` bit-string more carefully.  There are two kinds of
`1` bits in our bit-string.  All the `1` bits come in pairs.

In the scheme we just discussed earlier, the first of each pair marked by `(`
takes our state from `1` to `0` to indicate we have entered the quoted state
and the second of each pair marked by `)` takes out state from `0` to `1` to
indicate we have exited the quoted state.


```text
quotes:   0000000000000(0)(00)0(00000)0(0000)
```

Let's say we somehow have a way to split the `quotes` bit-string into two
separate bit-string very efficiently into an `enters` bit-string and
and `exits` bit-string. Does that put us in a better position?

```text
quotes:   0000000000000(0)(00)0(00000)0(0000)
enters:   00000000000001001000010000000100000
exits:    00000000000000010001000000010000001
mask:     1111111111111?0??00?1?00000?1?0000?
```

Sadly, the path forward still seems unclear.

What if we invert all the bits in `exits` to produce a new bit-string `~exits`?

```text
quotes:   0000000000000(0)(00)0(00000)0(0000)
enters:   00000000000001001000010000000100000
exits:    00000000000000010001000000010000001
~exits:   11111111111111101110111111101111110
mask:     1111111111111?0??00?1?00000?1?0000?
```

Hmmm.  It looks better because it seems closer to the desired bits in `mask`.

But the improvement is superficial.  I've flipped some bits that needed to be a `1` to
the desired value, but at a cost of flipping some other bits that need to be a `0`
into a `1` as well.

I've flipped too much.  Perhaps I am no closer after all.

But wait!  I've not used `enters` yet!

I may have flipped too much, but enters tells me exactly from which point where
I could compensate by flipping the following bits back to `0`.

What operation could possibly do that?

What I have are a bunch of consecutive ones separated by one or more `0`s.  The
bits I need to flip always form a zero more lengthed suffix of these consecutive
`1`s.  And the `enters` bit-string tells me the starting position of all those
flips!

There is an operation that allows me to flip runs of `1` bits and it is the humble
addition operator `+`.

If I add `enters` to `~exits`, the `1` bits in `enters` will cause
a cascade of carries starting from their position through the runs of `1`s in
`~exits` until the next `0`, where it will drop a `1` and the carries will
terminate.

Let's try that and see what we get:

```text
quotes:         0000000000000(0)(00)0(00000)0(0000)
enters:         00000000000001001000010000000100000
exits:          00000000000000010001000000010000001
~exits:         11111111111111101110111111101111110
enters+~exits:  11111111111110010001100000011000001
mask:           1111111111111?0??00?1?00000?1?0000?
```

Like magic, the erroneously flipped bits corrected themselves, and all the bits that
I care about match between `enters+~exits` and `mask`.

This means `enters+~exits` can serve as our mask.

So let's perform this masking operation on our rank-select bit-strings:

```text
text:             aaa,bbb,ccc␍␊"a""aa","b␍␊bb","c,cc"
markers:          000100010000100000001000*000100*000
newlines:         000000000000100000000000*0000000000
enters+~exits:    11111111111110010001100000011000001
masked-markers:   00010001000010000000100000001000000
masked-newlines:  00000000000010000000000000000000000
```

Voilà!

We've successfully cleared the erroneously set bits at the
positions marked by `*` and we have the correct rank-select bit-strings with
which we can parse all our RFC4180 compliant CSV files without loss of
data-parallelism!

## Not so fast

I mentioned briefly in order to pull this off I needed an operation that could
split my `quotes` bit-string into separately into two bit-strings that marked
the first and second bits of each pair of `1` bits in quotes:

```text
quotes:   0000000000000(0)(00)0(00000)0(0000)
enters:   00000000000001001000010000000100000
exits:    00000000000000010001000000010000001
```

Without this, all things fall apart.

But do not fear.  Next week, I will [explain how to pull this off][1].  Stay tuned!

[1]: ../posts/2018-08-22-pdep-and-pext-bit-manipulation-functions.html#splitting-a-bit-string-to-odd-and-even-bits
