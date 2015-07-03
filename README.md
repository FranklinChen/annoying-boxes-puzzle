# Solve an annoying boxes puzzle

[![Build Status](https://travis-ci.org/FranklinChen/annoying-boxes-puzzle.png)](https://travis-ci.org/FranklinChen/annoying-boxes-puzzle)

Solve this [annoying boxes puzzle](http://blog.plover.com/math/logic/annoying-boxes.html):

```text
There are two boxes on a table, one red and one green. One
contains a treasure. The red box is labelled "exactly one of the
labels is true". The green box is labelled "the treasure is in
this box."

Can you figure out which box contains the treasure?
```

## The followup blog post with solution

Read Mark-Jason Dominus's [writeup of the solution](http://blog.plover.com/math/logic/annoying-boxes-solution.html).

## Print solution

I implemented the solution in Haskell.

You can run it with the GHCi interpreter with:

```console
$ runhaskell src/AnnoyingBoxesPuzzle.hs
```

Or you can compile to native code and run that (overkill for this
little program with no external library dependencies):

```console
$ cabal build
$ cabal run
```

The output is crude but should be self-explanatory.

## Explanation of the solution

TODO
