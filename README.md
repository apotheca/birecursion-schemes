# Birecursion Schemes

This is a library for mono-, di- and bi- recursion schemes.

By extending recursion to functors with base bi-functors, we can build recursive stacks that allow you to perform actions while mapping over the functor content.

This is especially useful for partial / distributed data structures.

Please see the `haddock` docs for more detail.

> WARNING: This library is experimental, and only includes the core concepts. It has been published so that the core concepts may be discussed.

## Flags

### no-recursion-schemes

This library depends on `recursion-schemes`, but may be compiled without it using the `no-recursion-schemes` flag.

```haskell
cabal repl birecursion-schemes -f no-recursion-schemes
```
