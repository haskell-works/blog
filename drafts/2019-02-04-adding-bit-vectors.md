---
title: Adding bit vectors
author: John Ky
---

In the [previous a post][1], I described how addition can flip runs of bits.
This will explain how to do that efficiently.



# Closing Remarks

This post looked at how we can resegment our lazy bytestring to make the chunk sizes compatible with
SIMD instructions at a reasonable cost.

The next post will look at using FFI to call into C functions that use SIMD to do the heavy lifting.

[1]: ../posts/2018-08-15-data-parallel-rfc-compliant-csv-parsing.html



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
