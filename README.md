# CAL lookup table code generation

This `cal-lookuptable-gen` program generates a lookup table for the
CAL programming language from a given input range of integers and a
lookup function defined in Haskell. To use, modify `Example.hs` to
define your `Integer -> String` lookup function and the input
range. Then compile with `cabal install`. To use:

```
$ cal-lookuptable-example --help
cal-lookuptable-gen, (C) Rob Stewart, Rathlin Project 2015

cal-lookuptable-example [OPTIONS]
  Generates CAL implementation of a Haskell defined lookup function

Common flags:
  -h --help             Display help message
```
