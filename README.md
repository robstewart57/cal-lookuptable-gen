# CAL lookup table code generation

This `cal-lookuptable-gen` program generates an integer to integer
lookup table for the CAL programming language from a given input range
and a lookup function defined in Haskell. The Haskell platform is
required for in

To use, modify `Main.hs` to define your `Integer -> Integer` lookup
function and the input range. Then compile with `cabal install`. To
use:

```
$ cal-lookuptable-gen --help
cal-lookuptable-gen, (C) Rob Stewart, Rathlin Project 2015

cal-lookuptable-gen [OPTIONS]
  Generates CAL implementation of a Haskell defined lookup function

Common flags:
  -f --function         generates a function lookup with if/else statements
  -p --procedure        generates a procedure lookup with if/end statements
  -a --array            generates a procedure lookup with array indexing
  -h --help             Display help message
```
