---
title: Introduction to SIMD with linecount
author: John Ky
---

In a [previous post][1] I talked about using broadword techniques to
create a rank-select bit-string from text.

This post will explore using Single Instruction, Multiple Data (SIMD)
instructions to achieve the same thing.

The exploration will benchmark the SIMD, broadword and naive
implementations of line count to illustrate why
SIMD instructions are important to improving parsing performance.

# Creating a rank-select bit-string with SIMD

Recall from the earlier post that we can build a rank-select
bit-string from a piece of text like this:

```text
"name","age","profession"␤John,30,Code Monkey␤Kyle,40,Data Scrubber
0000000000000000000000000100000000000000000001000000000000000000000
```

Here, I mark every bit that corresponds to a newline character
at the corresponding position in the text.

From this bit-string, we can use the `popCount` operation to add
up the number of 1-bits in the string to arrive at the line count
for our text.

Recall that we were able to use broadword programming techniques
to do this conversion:

```haskell
testWord8s :: Word64 -> Word64
testWord8s w =  let w8s = w
                    w4s = w8s .|. (w8s .>. 4)
                    w2s = w4s .|. (w4s .>. 2)
                    w1s = w2s .|. (w2s .>. 1)
                in  pext w1s 0x0101010101010101
{-# INLINE testWord8s #-}
```

This approach allowed us to process 8 bytes at a time at the cost of
three OR `(.|.)`, three SHIFT `(.>.)` and one `pext` operations, all
of which are very cheap.

Whilst, broadword techniques allowed us to do this 8 bytes at a time,
SIMD instructions that exist on some generations of CPUs can help us
build our rank-select bit-string in a way that lets us process 16,
32 or even 64-bytes at a time.

On my Macbook, I can run a command to determine what the features my
CPU has:

```
$ sysctl -a | grep cpu | grep features:
machdep.cpu.features: FPU VME DE PSE TSC MSR PAE MCE CX8 APIC SEP MTRR PGE MCA CMOV PAT PSE36 CLFSH DS ACPI MMX FXSR SSE SSE2 SS HTT TM PBE SSE3 PCLMULQDQ DTES64 MON DSCPL VMX SMX EST TM2 SSSE3 FMA CX16 TPR PDCM SSE4.1 SSE4.2 x2APIC MOVBE POPCNT AES PCID XSAVE OSXSAVE SEGLIM64 TSCTMR AVX1.0 RDRAND F16C
machdep.cpu.leaf7_features: SMEP ERMS RDWRFSGS TSC_THREAD_OFFSET BMI1 HLE AVX2 BMI2 INVPCID RTM SMAP RDSEED ADX IPT SGX FPU_CSDS MPX CLFSOPT
machdep.cpu.extfeatures: SYSCALL XD 1GBPAGE EM64T LAHF LZCNT PREFETCHW RDTSCP TSCI
```

From this list, I know that my CPU supports the AVX2 instruction set which makes available
CPU intrinsics such as [`_mm256_cmpeq_epi8`][2] asn [`_mm256_movemask_epi8`][3].

With 256-bit registers, we're in a position to improve our parallelism
from 8 to 32 bytes at a time.

Now let's starting indexing out text.

In C, we first need to initialise a SIMD register to contain 32 copies of our
delimiter byte (in this case newline characters):

```c
__m256i ws_newline = broadcast_u8((uint8_t)'\n');
```

then we loop over the bytes 32-bytes at a time and perform a parallel
comparison on each chunk:

```c
char *text          = ...; // The text to parse
size_t text_length  = ...; // The length of the text to parse
uint32_t *target    = ...; // The buffer for the rank-select bit-string we are building

for (size_t i = 0; i != bytes_end; i += 32) {
  __m256i matches_bytes = _mm256_cmpeq_epi8(*(__m256i*)(text + i), ws_newlines);

  int matches_bits = _mm256_movemask_epi8(matches_bytes[3]);

  target[i / 32] = (uint32_t)matches_bits;
}
```

The above code compares each byte in `ws_newlines` with the corresponding
byte in `(__m256i*)text`. 

`matches_bytes` will contain 32 bytes, each of which will hold one of two
values: `0x00` or `0xff` dependending on whether the corresponding byte in
`(__m256i*)text` was equal to the corresponding by in `matches_bytes`.

The [`_mm256_movemask_epi8`][3] function is then used to compress this result
by taking the high bit of each byte in `matches_bytes` and packing them
into `matches_bits`.

Compared to the broadword implementation, this reduces the number of
load/stores by a factor of 8 and the number of
register instructions for a 32-byte chunk from 56 to 2.

# Benchmarking

To look at the potential performance gains from using SIMD versus other
alternatives, I've written three versions of a tool that counts the number
of lines in a text file: `naive`, `broadword` and `simd`

## Naive

The naive version does nothing more than traverse the text
byte-by-byte and compare each byte for equality and
incrementing a count on success:

```c
size_t process_data(char *text, size_t bytes_read)
{
  size_t popCount = 0;

  for (size_t i = 0; i < bytes_read; ++i)
  {
    if (text[i] == '\n') {
      ++popCount;
    }
  }

  return popCount;
}
```

## Broadword

The broadword version uses the XOR operation to perform parallel comparison of all 8 bytes in the
word with one instruction and relies on a small number of SHIFTS (`>>`) and ANDS (`&`) to
convert the result into the bit-string we need:

```c
size_t process_data(char *text, size_t bytes_read)
{
  size_t popCount = 0;
  char *mask = "\n\n\n\n\n\n\n\n";
  uint64_t wm = *(uint64_t *)mask;

  for (size_t i = 0; i < bytes_read; i += 8) {
    if (i + 8 <= bytes_read) {
      uint64_t w0 = ~(*(uint64_t *)(text + i) ^ wm);
      uint64_t w1 = (w0 >> 4) & w0; // Not a typo.  See De Morgan's law.
      uint64_t w2 = (w1 >> 2) & w1;
      uint64_t w3 = (w2 >> 1) & w2;

      uint64_t bits = (uint64_t)_pext_u64(w3, 0x0101010101010101L);

      popCount += _popcnt64(bits);
    } else {
      for (size_t j = i; j < bytes_read; ++j) {
        if (text[j] == '\n') {
          ++popCount;
        }
      }
    }
  }

  return popCount;
}
```

The function falls back to byte-by-byte comparison if there are any bytes left
that cannot fill a 64-bit word.

## SIMD

The SIMD version requires a register to be initialised with the our delimiter
replicated to all bytes of the SIMD register in the `ws_newlines` argument
with the [`_mm256_set1_epi8`][6] intrinsic.

This value is then compared to the text 64-bytes at a time with the
[`_mm256_cmpeq_epi8`][2] intrinsic and summarised into a bit-string with the
[`_mm256_movemask_epi8`][3] intrinsic.

```c
size_t process_data(char *text, size_t bytes_read)
{
  __m256i ws_newlines = _mm256_set1_epi8('\n');
  char *bytes_end = text + bytes_read;
  size_t popCount = 0;

  if ((bytes_read & 0x1f) == 0) {
    for (; text != bytes_end; text += 32) {
      __m256i matches_bytes = _mm256_cmpeq_epi8(*(__m256i*)text, ws_newlines);

      int matches_bits = _mm256_movemask_epi8(matches_bytes);

      popCount += _mm_popcnt_u32((uint32_t)matches_bits);
    }
  } else {
    for (size_t i = 0; i < bytes_read; i += 32) {
      size_t bytes_read_iter = bytes_read - i;

      if (bytes_read_iter > 32) {
        bytes_read_iter = 32;
      }

      __m256i matches_bytes = _mm256_cmpeq_epi8(*(__m256i*)(text + i), ws_newlines);

      int matches_bits = _mm256_movemask_epi8(matches_bytes);

      popCount += _mm_popcnt_u32((uint32_t)((0xffffffff >> (32 - bytes_read_iter)) & (uint32_t)matches_bits));
    }
  }

  return popCount;
}
```

The function falls back to byte-by-byte comparison if there are any bytes left
that cannot fill a 256-bit SIMD register.

# Initial benchmark results

I benchmark the three versions of the code in C as well as `wc -l` as a baseline.

The source code for the benchmarks can be found [here][7] and the results [here][8].

The code is compiled with `-mavx2 -mbmi2` to enable access to the SIMD and
bit manipulation instructions I need.

```text
wc -l                   0m5.458s
./naive.auto.out        0m2.654s
./broadword.auto.out    0m2.963s
./simd.auto.out         0m1.759s
```

I find that `wc -l` is the slowest and the `simd` implementation the fastest
and the broadword implementation performs somewhere in between.

That much is as expected.

The surprise here is that the naive version runs faster than the broadword
version.

What is going on here?

Before jumping to conclusions,
let's take a look at the assembly code generated for the naive version:

```text
_process_data:                          ## @process_data
    .cfi_startproc
...
## BB#8:
    leaq            -1(%r8), %rdx
    subq            %rax, %rdx
    vpxor           %ymm8, %ymm8, %ymm8
    xorl            %eax, %eax
    vpbroadcastd    LCPI0_0(%rip), %xmm4 ## xmm4 = [10,10,10,10]
    vpbroadcastq    LCPI0_1(%rip), %ymm5 ## ymm5 = [1,1,1,1]
    vpxor           %ymm9, %ymm9, %ymm9
    vpxor           %ymm2, %ymm2, %ymm2
    vpxor           %ymm3, %ymm3, %ymm3
    .p2align 4, 0x90
...
    .cfi_endproc
```

If we lookup the Intel documentation for the [`vpbroadcastq`][4] and [`vpxor`][5]
instructions we find that these functions are SIMD instructions and that
`gcc` has auto-vectorised the naive implementation.

The naive implementation was in fact elaborately auto-vectorised that a 14 line C
function became 152 lines of assembly code.

If we deny `gcc` the freedom to auto-vectorise the code with the `-fno-tree-vectorize`
flag we get `71` lines of assembly instead and suffer worse results by far:

```text
./naive.out           0m4.659s
./broadword.out       0m2.960s
./simd.out            0m1.703s
wc -l                 0m5.458s
```

GHC currently does not perform any auto-vectorisation, so I'd expected the naive
version when written in Haskell would perform no better than the naive C
implementation without auto-vectorsation.

# Closing remarks

The benchmarks make a compelling case for using SIMD instructions
where ever possible and broadword where the target CPU architecture does
not support the SIMD instructions we need.

Unfortunately, GHC does not have native support for the SIMD instructions
we need.

In a future post I'll look at using these SIMD from GHC using Foreign
Function Interface (FFI) and addressing some of the challenges of using
SIMD with Haskell's lazy IO.

[1]: ../posts/2018-08-08-data-parallel-rank-select-bit-string-construction.html
[2]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,766&text=_mm256_cmpeq_epi8
[3]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,766,3612&text=_mm256_movemask_epi8
[4]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,534&text=vpbroadcastq
[5]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,534,5724&text=vpxor
[6]: https://software.intel.com/sites/landingpage/IntrinsicsGuide/#expand=765,767,802,4676&text=_mm256_set1_epi8
[7]: https://github.com/haskell-works/blog-examples/tree/3f65f1d6c4dac274e57b9ddcf4c610d321fb3234/linecount
[8]: https://github.com/haskell-works/blog-examples/commit/3f65f1d6c4dac274e57b9ddcf4c610d321fb3234

