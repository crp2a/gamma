# Extract or Replace Parts of an Object

Operators acting on objects to extract or replace parts.

## Usage

``` r
# S4 method for class 'GammaSpectrum'
x[[i]]

# S4 method for class 'GammaSpectra'
x[i, j]

# S4 method for class 'DoseRateModel'
x[[i]]

# S4 method for class 'CalibrationCurve'
x[[i]]

# S4 method for class 'PeakPosition'
x[[i]]
```

## Arguments

- x:

  An object from which to extract element(s) or in which to replace
  element(s).

- i, j:

  Indices specifying elements to extract or replace. Indices are
  [`numeric`](https://rdrr.io/r/base/numeric.html),
  [`integer`](https://rdrr.io/r/base/integer.html) or
  [`character`](https://rdrr.io/r/base/character.html) vectors or empty
  (missing) or `NULL`. Numeric values are coerced to
  [`integer`](https://rdrr.io/r/base/integer.html) as by
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero). Character vectors will be matched to the name
  of the elements. An empty index (a comma separated blank) indicates
  that all entries in that dimension are selected.

## Value

A subsetted object.

## See also

Other mutator:
[`mutator`](https://crp2a.github.io/gamma/reference/mutator.md)

## Author

N. Frerebeau
