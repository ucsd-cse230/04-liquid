# Extra Credit: Refinement Types

## Introduction

In this exercise, you will write a function that sorts a list of values,
and use LiquidHaskell's refinement types to verify your code, i.e. to prove
that

1. The output list is sorted,
2. The output list has the same elements as the input list.

## Run LiquidHaskell

Simply by doing

```bash
$ stack build
```

(or it should be running automatically on save in VSCode, modulo some bugs and glitches...)

## `BSTSort.hs`

All the code is in `BSTSort.hs`

## Problem 1: `toList`

First, complete the implementation of `toList` by filling in the code for the `Node v l r` case.
When you are done the code should typecheck. Note that this requires you to ensure that `toList`
produces an ordered list `OList a` as output, so you may have to write some *helper* functions.

## Problem 2: `fromList`

Next, complete the implementation of `fromList` by filling in the code for the `x:xs` case.
When you are done the code should typecheck. Note that this requires you to ensure that the
constructed tree is in fact a "binary search tree" as captured by the the definition for `BST`
up at the top. Again, you may need to write some helper code, e.g. to `insert` an element into
a `BST a`.

## Problem 3: `bstSort`

Finally, you need to *verify* that the given implementation of `bstSort` correctly implements
the type specification which says that the output is ordered *and* has the same elements as the
input list `xs`. To do so, fill in the correct *specifications* for `toList` and `fromList` (and
any helper functions you may have written) so that the given implementation of `bstSort` (and all the
code) is verified by LiquidHaskell.