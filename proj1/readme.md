# Scheme language

## Introduction
[Scheme](http://www.schemers.org/) is a programming language that supports multiple paradigms, including functional programming and imperative programming, and is one of the two main dialects of Lisp.

## Implementation of scheme
There is three main scheme implementation
   [Chez scheme](https://github.com/cisco/ChezScheme),
   [Racket](https://racket-lang.org/),
   [Chicken](https://www.call-cc.org/).
   Among these, racket provides a GUI and will be easy to use.

## Test
They both provide REPL(Read–eval–print loop) where you can test each function.
For example, in "bst.scm", we can insert-list function and racket will return the result.
![p1](/media/wu/software/file/homework/ads/proj1/p2.png)

## Our program
Our splay.exe, bst.exe, avl.exe will read "xxx-test.txt" and output the result.
0 stands for insert, 1 for delete and 2 for search.
