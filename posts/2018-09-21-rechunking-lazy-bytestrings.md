---
title: Rechunking lazy bytestrings
author: John Ky
---

In the [previous post][1], we've established that we want to use SIMD for speed.

We'd also like our CSV parser stream the data to avoid excessive memory usage
so we're going to have to read the CSV input in chunks.

Given that SIMD registers are currently up to 512-bits in size, the chunk
size will need to be multiples of 64-bytes to work with arbitrary SIMD
instructions.

This post will look at the chunk size Haskell's [`bytestring`][2] library actually
gives us and  explore some ways we can get the required chunk size we need.

# Lazy IO

Due to laziness, streaming in Haskell is straightforward.  The following
function lazily reads the entire contents of the input file and writes
them into the output file.

```haskell
import qualified Data.ByteString.Lazy as LBS

cat :: FilePath -> FilePath -> IO ()
cat inputFile outputFile = do
  bs <- LBS.readFile inputFile
  LBS.writeFile outputFile bs
```

For efficiency, the lazy bytestring is actually very similar in structure to
a list of strict bytestrings.  Each bytestring represents a chunk of the
input file contents with a [carefully chosen chunk size][3] to maximise performance.

```haskell
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
```

Evidence of this behaviour is observable by using the [`toChunks`][4]
function to convert the lazy bytestring and inspecting their size.

The following command reads a file with lazy IO and counts the frequency of each
chunk size:

```haskell
$ git clone git@github.com:haskell-works/hw-simd-cli.git
$ cd hw-simd-cli
$ git checkout 2a9dadf9291cad68f2b05917619861eead3c31dd
$ ./project.sh install
$ hw-simd chunks -i ~/7g.csv -m chunked -r classic
Total chunks: 232230
Chunk histogram:
2350,1
32752,232229
```

Unfortunately for us, this chunk size is no good for using SIMD instructions that
can use registers up to 512-bits or 64-bytes.

More interestingly, even had the `defaultChunkSize` been set to a more
convenient size of being a multiple of 64-bytes, a 64-byte multiple chunk size is
not guaranteed, as shown in this example where we read from standard input instead:

```haskell
$ cat ~/7g.csv | time hw-simd chunks -i - -m chunked -r classic
Total chunks: 279332
Chunk histogram:
16,5780
32,5882
48,1793
64,739
80,400
96,239
112,130
128,77
144,55
160,50
176,32
192,20
208,16
224,12
240,5
256,7
272,9
288,8
304,8
320,2
336,5
368,3
384,2
400,1
416,1
432,3
448,1
464,1
480,1
496,4
512,1
544,3
560,3
592,1
640,1
704,2
1088,1
1184,1
1280,1
16384,8555
16400,10125
16416,23140
16432,9269
16448,4729
16464,2883
16480,1683
16496,1005
16512,669
16528,433
16544,303
16560,202
16576,160
16592,95
16608,86
16624,60
16640,53
16656,36
16672,43
16688,29
16704,38
16720,23
16736,13
16752,13
16768,20
16784,15
16800,8
16816,14
16832,9
16848,8
16864,7
16880,7
16896,2
16912,4
16928,9
16944,11
16960,10
16976,9
16992,11
```

# Rechunking & resegmenting

One strategy we could use to ensure our bytestrings are always chunked to 64-byte multiples is
to [`rechunk`][5] the bytestrings into equal chunk sizes like the following:

```text
|---------------a---------------|---------b----------|-c-|-------------d---------------|
|-e--|-f--|-g--|-h--|-i--|-j--|=k==|-l--|-m--|-n--|=o==|=p==|-q--|-r--|-s--|-t--|-u--|v|
```

In the above, the chunks `e`-`j`, `l`-`n`, and `q`-`v` don't require any byte copying because
they are strict substrings of the chunks `a`, `b` and `d` respectively.

`k`, `o`, and `p` however do require copy because their bytes come from multipe source chunks.

The need for copying is denoted by using the `=` characters.

The above scheme may minimise the amount of byte copying, but it is still fairly expensive
because many bytestring objects are created.

To reduce the number of bytestring objects, another approach is to [`resegment`][6] the data
instead.

This process is shown below:

```text
|---------------a---------------|---------b----------|-c-|-------------d---------------|
|--------------w--------------|=k==|------x-------|=o==|=p==|-----------y------------|v|
```

Here, segments `v`, `w` and `y` are substrings of `a`, `b` and `d` respectively with a size
that is the largest multiple of the chunk size allowed by the source segments.  The segments
are equivalent to the concatenation of the `e`-`j`, `l`-`n`, and `q`-`v` chunks in the
`rechunk` example.

This gets us to the point where all except the last segment is a multiple of the chunk size.

For doing our SIMD operations, we'd like all the segments to be a multiple of the chunk size
so [`resegmentPadded`][7] will pad the last segment to the chunk size with 0 bytes:

```text
|---------------a---------------|---------b----------|-c-|-------------d---------------|
|--------------w--------------|=k==|------x-------|=o==|=p==|-----------y------------|=z==|
```

This padded segment denoted `z` will require byte copying from `d` and zero-filling the remaining
buffer to the chunk size.

For clarity, I provide the diagrams for each strategy side-by-side:

```text
rechunk:          |-e--|-f--|-g--|-h--|-i--|-j--|=k==|-l--|-m--|-n--|=o==|=p==|-q--|-r--|-s--|-t--|-u--|v|
resegment:        |--------------w--------------|=k==|------x-------|=o==|=p==|-----------y------------|v|
resegmentPadded:  |--------------w--------------|=k==|------x-------|=o==|=p==|-----------y------------|=z==|
```

Some benchmarking will give us some idea of how much rechunking and resegmenting costs us:

```bash
$ git clone git@github.com:haskell-works/hw-simd-cli.git
$ cd hw-simd-cli
$ ./project.sh install
$ cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m default > /dev/null
7.08GiB 0:00:05 [1.27GiB/s]
$ cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m rechunk -c 64 > /dev/null
7.08GiB 0:00:22 [ 317MiB/s]
$ cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m resegment -c 64 > /dev/null
7.08GiB 0:00:06 [1.15GiB/s]
```

The results show the cost of using small chunks is drastic compared to the much
more modest overhead of resegmenting.

# Pre-chunked reading

An alternative to resegmenting the lazy bytestring is to read the bytes with the desired
segment size in the first place.

The [`hGetContents`][8] function from the [`bytestring`][2] library is implemented in terms of
[`hGetContentsN`][9] like this:

```haskell
hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k -- only blocks if there is no data available
        if S.null c
          then hClose h >> return Empty
          else do cs <- lazyRead
                  return (Chunk c cs)
```

[`hGetContentsChunkedBy`][13], a different version of the function which guarantees
that every chunk is the same size (except the last) can be implemented by using
[`hGetBuf`][10] and [`createAndTrim`][11] instead of [`hGetSome`][12] and keeping
everything else the same:

```haskell
hGetContentsChunkedBy :: Int -> IO.Handle -> IO LBS.ByteString
hGetContentsChunkedBy k h = lazyRead
  where lazyRead = IO.unsafeInterleaveIO loop
        loop = do
            c <- BS.createAndTrim k $ \p -> IO.hGetBuf h p k
            if BS.null c
              then IO.hClose h >> return LBS.Empty
              else LBS.Chunk c <$> lazyRead
```

Benchmarking this shows the performance is comparable to resegmenting.

```bash
$ for x in {1..5}; do cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m default > /dev/null; done
7.08GiB 0:00:06 [1.10GiB/s]
7.08GiB 0:00:05 [1.26GiB/s]
7.08GiB 0:00:05 [1.29GiB/s]
7.08GiB 0:00:05 [1.27GiB/s]
7.08GiB 0:00:05 [1.27GiB/s]
$ for x in {1..5}; do cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m prechunk -c 32704 > /dev/null; done
7.08GiB 0:00:06 [1.12GiB/s]
7.08GiB 0:00:06 [1.16GiB/s]
7.08GiB 0:00:06 [1.10GiB/s]
7.08GiB 0:00:06 [1.13GiB/s]
7.08GiB 0:00:06 [1.14GiB/s]
$ for x in {1..5}; do cat ~/7g.csv | pv -t -e -b -a | hw-simd cat -i - -o - -m resegment -c 64 > /dev/null; done
7.08GiB 0:00:06 [1.14GiB/s]
7.08GiB 0:00:06 [1.16GiB/s]
7.08GiB 0:00:06 [1.06GiB/s]
7.08GiB 0:00:05 [1.18GiB/s]
7.08GiB 0:00:05 [1.19GiB/s]
```

This is likely because the chunks returned by `hGetContents` were already large enough that the extra effort
to resegment the lazy bytestring is negligible.

# Closing Remarks

This post looked at how we can resegment our lazy bytestring to make the chunk sizes compatible with
SIMD instructions at a reasonable cost.

The next post will look at using FFI to call into C functions that use SIMD to do the heavy lifting.


[1]: ../posts/2018-09-03-simd-with-linecount.html
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
