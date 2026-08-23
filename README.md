# Reduction Semantics and How to Read Them

This repository contains the source code and Redex models for a talk from PLMW at ICFP 2026.

In a recent version of Racket, use this command to install the package

```sh
$ raco pkg install plmw-2026
```
and then this command to see the slides:
```sh
$ rhombus -l plmw-2026/slides
```

## Talk Abstract

The semantics of a programming language is often given by defining a set of simplification rules (e.g., `1+1` simplifies to `2`, `(car (cons 1 '())` simplifies to `1`) and a set of contexts where these simplifications are allowed to occur (e.g., in the test position of an `if`, but not in the branches). These steps are then chained together into reduction sequences that start from an arbitrary program in the language and end with the program's final result (or never end, if the program runs forever).

This style of semantics is able to describe a wide range of languages, from simple ones that are little more than arithmetic to sophisticated ones with fancy forms of control flow. Critical to the ease and concision is the notion of an evaluation context which, roughly speaking, captures where the simplification steps are allowed to occur via productions in a grammar.

In this talk, I will give an overview of the way these semantics are defined and a sense of the commonly used patterns. We will see how different orders of evaluation, underspecification of the language, and complex control flow (e.g., exceptions and continuations) are all just a matter of using the basic tools in creative ways. By the end of the talk, you should be able to get a basic understanding of the semantics of a language in paper by studying its reduction relation's rules and working through a few examples.
