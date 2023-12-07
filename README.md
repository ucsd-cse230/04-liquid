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
$ stack build --fast
```

The first time, it will take a **long time** to install LiquidHaskell (the refinement type checker).

You _should_ get a bunch of LH errors in VSCODE itself, but if its being flaky,
then open up a terminal and just type

```bash
$ stack build --fast --file-watch
```

and it will automatically recompile at each save, and show you errors in the terminal.

Your job is to get all the code to compile AND then ultimately get

```bash
$ stack test --fast
```

to produce the following output:

```bash
OVERALL SCORE = 45 / 45
```

(or it should be running automatically on save in VSCode, modulo some bugs and glitches...)

## Part A : `BSTSort.hs`


### Problem 1: `toList`

First, complete the implementation of `toList` by filling in the code for the `Node v l r` case.
When you are done the code should typecheck. Note that this requires you to ensure that `toList`
produces an ordered list `OList a` as output, so you may have to write some *helper* functions.

### Problem 2: `fromList`

Next, complete the implementation of `fromList` by filling in the code for the `x:xs` case.
When you are done the code should typecheck. Note that this requires you to ensure that the
constructed tree is in fact a "binary search tree" as captured by the the definition for `BST`
up at the top. Again, you may need to write some helper code, e.g. to `insert` an element into
a `BST a`.

### Problem 3: `bstSort`

Finally, you need to *verify* that the given implementation of `bstSort` correctly implements
the type specification which says that the output is ordered *and* has the same elements as the
input list `xs`. To do so, fill in the correct *specifications* for `toList` and `fromList` (and
any helper functions you may have written) so that the given implementation of `bstSort` (and all the
code) is verified by LiquidHaskell.

## Part B : `Pointers.hs`

See [Chapter 11 of these notes](https://ucsd-progsys.github.io/liquidhaskell-tutorial/book.pdf) for more documentation.


### Problem 1: `create`

See Exercise 11.2 (Create) in the [notes].

### Problem 2: `pack`

See Exercise 11.4 (`pack`) in the [notes].

### Problem 3: `unsafeTake` and `unsafeDrop`

See Exercise 11.5 in the [notes].

### Problem 4: `unpack`

See Exercise 11.6 in the [notes].




See Exercise 11.2 (Create) in the [notes].
