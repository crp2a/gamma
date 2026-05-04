# Common Operations on GammaSpectrum Objects

Performs common operations on `GammaSpectrum` objects.

## Usage

``` r
# S4 method for class 'GammaSpectrum,GammaSpectrum'
Arith(e1, e2)

# S4 method for class 'GammaSpectrum,numeric'
Arith(e1, e2)

# S4 method for class 'GammaSpectrum,GammaSpectrum'
Compare(e1, e2)

# S4 method for class 'GammaSpectrum,numeric'
Compare(e1, e2)

# S4 method for class 'GammaSpectrum,GammaSpectrum'
Logic(e1, e2)

# S4 method for class 'GammaSpectrum,numeric'
Logic(e1, e2)

# S4 method for class 'GammaSpectrum,logical'
Logic(e1, e2)

# S4 method for class 'GammaSpectrum'
Math(x)

# S4 method for class 'GammaSpectrum'
Math2(x, digits)

# S4 method for class 'GammaSpectrum'
Summary(x, ..., na.rm = FALSE)
```

## Arguments

- x, e1, e2:

  An object (typically a
  [GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
  object).

- digits:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the number of digits to be used in
  [`round()`](https://rdrr.io/r/base/Round.html) or
  [`signif()`](https://rdrr.io/r/base/Round.html).

- ...:

  Further arguments passed to or from methods.

- na.rm:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should
  missing values (including `NaN`) be omitted from the calculations?

## Group Generics

[GammaSpectrum](https://crp2a.github.io/gamma/dev/reference/GammaSpectrum-class.md)
objects have support for S4 group generic functionality to operate
within elements across objects:

- `Arith`:

  "`+`", "`-`", "`*`", "`^`", "`\%\%`", "`\%/\%`", "`/`"

- `Compare`:

  "`==`", "`>`", "`<`", "`!=`", "`<=`", "`>=`"

- `Logic`:

  "`&`", "`|`"

- `Math`:

  "`abs`", "`sign`", "`sqrt`", "`ceiling`", "`floor`", "`trunc`",
  "`cummax`", "`cummin`", "`cumprod`", "`cumsum`", "`log`", "`log10`",
  "`log2`", "`log1p`", "`acos`", "`acosh`", "`asin`", "`asinh`",
  "`atan`", "`atanh`", "`exp`", "`expm1`", "`cos`", "`cosh`", "`cospi`",
  "`sin`", "`sinh`", "`sinpi`", "`tan`", "`tanh`", "`tanpi`", "`gamma`",
  "`lgamma`", "`digamma`", "`trigamma`"

- `Math2`:

  "`round`", "`signif`"

- `Ops`:

  "`Arith`", "`Compare`", "`Logic`"

- `Summary`:

  "`min`", "`max`", "`range`", "`prod`", "`sum`", "`any`", "`all`"

## Author

N. Frerebeau

## Examples

``` r
## No examples
```
