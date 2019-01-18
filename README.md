Arbitrary-precision arithmetic package for Elm

# chain-partners/bignum

Elm library for arbitrary-precision decimal arithmetic that supports basic arithmetic, comparison, and rounding operations.

## Design Goals

I initially wrote this library because I needed division operation of big decimal numbers. My goal was to write a library that:

1. Supports basic arithmetic operations
2. Has good enough performance on browser

## TODO

* Improve handling of fraction digits that are dropped off
* Get rid of artificial `Maybe.withDefault` calls
* Improve performance, especially for division and square root operations
* Add base conversion, including experimentation with changing default base to 2^26
* Reimplement `toString` functions using `Parser` library
* Find a better way to handle `Significand` without relying on `Integer.toString`. Recursion was far too slower than string manipulation using regular expression.
