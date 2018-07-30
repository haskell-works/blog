---
title: Introduction to the succinct semi-index
author: John Ky
---

# Balanced Parenthesis Index

The rank select bit string will allow us to jump to the relevant n-th node in
the document, but it won't allow us to navigate the document as a tree because
it doesn't capture the parent/sibling relationship of the nodes.

This will require another kind of index called the balanced parenthesis index,
which is capable of faithfully representing the structure of a tree.

```json
[{ "name": "John", "age": 30 }, { "name": "Kyle", "age": 31 }]
```

```text
                             [ ]
                              |
               +--------- ----+---------------+
               |                              |
              { }                            { }
               |                              |
   +--------+--+----+-----+       +-------+---+----+-----+
   |        |       |     |       |       |        |     |
"name"   "John"   "age"   30   "name"   "Kyle"   "age"   31
```

```text
                             ( )
                              |
               +--------- ----+---------------+
               |                              |
              ( )                            ( )
               |                              |
   +--------+--+----+-----+       +-------+---+----+-----+
   |        |       |     |       |       |        |     |
  ( )      ( )     ( )   ( )     ( )     ( )      ( )   ( )
```

```json
[{ "name": "John", "age": 30 }, { "name": "Kyle", "age": 31 }]
(( ()      ()      ()     () )  ( ()     ()       ()     () ))
```

```haskell
λ> let fc = firstChild
λ> let ns = nextSibling
λ> let c = Cursor "((()()()())(()()()()))" 1
λ> mapM_ printCursor $ ($ c) $ return
((()()()())(()()()()))
^
λ> mapM_ printCursor $ ($ c) $ return >=> fc
((()()()())(()()()()))
 ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> fc
((()()()())(()()()()))
  ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> fc >=> ns
((()()()())(()()()()))
    ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> fc >=> ns >=> ns
((()()()())(()()()()))
      ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> fc >=> ns >=> ns >=> ns
((()()()())(()()()()))
        ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> ns
((()()()())(()()()()))
           ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> ns >=> fc
((()()()())(()()()()))
            ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> ns >=> fc >=> ns
((()()()())(()()()()))
              ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> ns >=> fc >=> ns >=> ns
((()()()())(()()()()))
                ^
λ> mapM_ printCursor $ ($ c) $ return >=> fc >=> ns >=> fc >=> ns >=> ns >=> ns
((()()()())(()()()()))
                  ^
```

# Ingredients to making this fast

* Fast way to build a Rank Select Bit String Index for a document
* Fast way to build a Balanced Parentheses Index for a document
* Fast rank/select operations
* Fast firstChild/nextSibling operations
* Fast value parsers

Ideally, we'd like to build a parser made of these components, and be faster
than a traditional whole document parser while using less memory.
