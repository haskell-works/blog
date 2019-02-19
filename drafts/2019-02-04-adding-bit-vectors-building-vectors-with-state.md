---
title: Adding bit vectors - Building Vectors with State
author: John Ky
---

# Adding bit-vectors

Now we can use our `sumCarry` function to implement bit-vector
addition by 

For example we show how to add two bit-vectors `a` and `b` in the following
diagram:

```text
a = 128 │ 00010001   00011111   10001000   00000000
b = 128 │ 00010001   11111000   00011111   11000000
carry   │ 
a + b   │ 
```

First initialise the carry to `0x0`:

```text
a = 128 │ 00010001   00011111   10001000   00000000
b = 128 │ 00010001   11111000   00011111   11000000
carry   │ 00000000
a + b   │ 
```

Then perform the addition of the first word and compute
the next carry with our `sumCarry` function:

```text
a = 128 │ 00010001─┐ 00011111   10001000   00000000
b = 128 │ 00010001─┤ 11111000   00011111   11000000
carry   │ 00000000─┴─10000000
a + b   │ 00001000
```

Notice the `1` bit in the next carry.

We can then repeat this for the next set of words:

```text
a = 128 │ 00010001─┐ 00011111─┐ 10001000   00000000
b = 128 │ 00010001─┤ 11111000─┤ 00011111   11000000
carry   │ 00000000─┴─10000000─┴─10000000
a + b   │ 00001000   00011000
```

Above we can see the carry bit trigger a series of
bit flips from the beginning of the word.

We continue again:

```text
a = 128 │ 00010001─┐ 00011111─┐ 10001000─┐ 00000000
b = 128 │ 00010001─┤ 11111000─┤ 00011111─┤ 11000000
carry   │ 00000000─┴─10000000─┴─10000000─┴─10000000
a + b   │ 00001000   00011000   01010000
```

And again:

```text
a = 128 │ 00010001─┐ 00011111─┐ 10001000─┐ 00000000
b = 128 │ 00010001─┤ 11111000─┤ 00011111─┤ 11000000
carry   │ 00000000─┴─10000000─┴─10000000─┴─10000000
a + b   │ 00001000   00011000   01010000   00100000
```

Until we're done.

The code that implements this follows:

```haskell
sumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
sumVector u v = DVS.create $ do
  w <- DVSM.new len
  go w 0 False
  return w
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Bool -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = sumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return 1
```

The `sumVector` function takes the two bit-vectors to add and an initial carry,
