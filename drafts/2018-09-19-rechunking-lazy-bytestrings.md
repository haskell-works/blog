---
title: Rechunking lazy bytestrings
author: John Ky
---

In the previous post, I've established that I want to use SIMD for speed.

I'd also like my CSV parser stream the data to avoid excessive memory usage
so I'm going to have to read my CSV input in chunks.

Given that SIMD registers are currently up to 512-bits in size, the chunk
size will need to be multiples of 64-bytes.

This post will look at chunk size Haskell's library actually gives us
explore some ways I can get the required chunk size I need.

# Lazy IO

Due to laziness, streaming in Haskell is straightforward.  The following
function function lazily reads the entire contents of the input file and
writes them into the output file.

```haskell
import qualified Data.ByteString.Lazy

cat :: FilePath -> FilePath -> IO ()
cat inputFile outputFile = do
  bs <- LBS.readFile inputFile
  LBS.writeFile outputFile bs
```

For efficiency, the lazy bytestring is actually very similar in structure to
a list of strict bytestrings.  Each bytestring represents a chunk of the
input file contents with a carefully chosen chunk size to maximise performance.

```haskell
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
```

Evidence of this behaviour is observable by using the `Data.ByteString.Lazy.toChunks`
function to convert the lazy bytestring and inspecting their sizes.

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

# Rechunking

One strategy we could use to ensure our bytestrings are always chunked to 64-byte multiples is
to rechunk the bytestrings.

In doing so, we'd want to reduce the amount of copying necessary.

Suppose in the following diagram that *A* represents 64-byte boundaries and *B* represents
the input data we want to rechunk.

```text
A |----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
B |---------------a---------------|---------b----------|-c-|-------------d---------------|
C |--------------e--------------|=f==|------g-------|=h==|=i==|-----------j------------|=k==|
```

We can perform the rechunking as in *C* to reuse the most amount of original chunk data as
possible.  Here, the bytestrings *e* *g* and *j* are strict substrings of *a* *b* and *d*
so they share the same buffers and no copying is necessary.  *f* *h* *i* and *k* are buffers
which straddle the chunk boundaries in the original bytestring *B* and require copying
to build appropriately aligned chunks.  The bytes in these chunks will need to be copied
so they are kept to the smallest size possible to avoid copying more than necessary.
The last chunk *k* is padded with zeros to ensure it is of a valid chunk size as well.


```haskell
resegmentPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
resegmentPadded multiple = go
  where go (bs:bss) = case BS.length bs of
              bsLen -> if bsLen < multiple
                then case multiple - bsLen of
                  bsNeed -> case bss of
                    (cs:css) -> case BS.length cs of
                      csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                      csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                      _     | otherwise       ->                            go ((bs <> cs)       :css)
                    [] -> [bs <> BS.replicate bsNeed 0]
                else case (bsLen `div` multiple) * multiple of
                  bsCroppedLen -> if bsCroppedLen == bsLen
                    then bs:go bss
                    else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
        go [] = []
```
