# Get or Set Parts of an Object

Getters and setters to extract or replace parts of an object.

## Usage

``` r
get_hash(x)

get_names(x)

set_names(x) <- value

get_energy_calibration(x)

set_energy_calibration(x) <- value

get_livetime(x)

get_realtime(x)

get_channels(x)

get_counts(x)

get_rates(x)

get_energy(x, ...)

set_energy(x) <- value

get_method(x)

set_method(x) <- value

get_residuals(x)

range_channels(x, ...)

range_energy(x, ...)

# S4 method for class 'GammaSpectrum'
length(x)

# S4 method for class 'GammaSpectrum'
get_hash(x)

# S4 method for class 'GammaSpectrum'
get_names(x)

# S4 method for class 'GammaSpectrum'
get_livetime(x)

# S4 method for class 'GammaSpectrum'
get_realtime(x)

# S4 method for class 'GammaSpectrum'
get_channels(x)

# S4 method for class 'GammaSpectrum'
get_counts(x)

# S4 method for class 'GammaSpectrum'
get_rates(x)

# S4 method for class 'GammaSpectrum'
get_energy(x)

# S4 method for class 'GammaSpectrum'
range_energy(x, na.rm = FALSE)

# S4 method for class 'GammaSpectrum'
range_channels(x, na.rm = FALSE)

# S4 method for class 'Baseline'
get_method(x)

# S4 method for class 'GammaSpectra'
get_hash(x)

# S4 method for class 'GammaSpectra'
get_names(x)

# S4 method for class 'GammaSpectra'
get_livetime(x)

# S4 method for class 'GammaSpectra'
get_realtime(x)

# S4 method for class 'GammaSpectra'
get_channels(x)

# S4 method for class 'GammaSpectra'
get_counts(x)

# S4 method for class 'GammaSpectra'
get_rates(x)

# S4 method for class 'GammaSpectra'
get_energy(x)

# S4 method for class 'GammaSpectra'
range_energy(x, na.rm = FALSE)

# S4 method for class 'GammaSpectra'
range_channels(x, na.rm = FALSE)

# S4 method for class 'DoseRateModel'
get_residuals(x)

# S4 method for class 'PeakPosition'
get_hash(x)

# S4 method for class 'PeakPosition'
get_channels(x)

# S4 method for class 'PeakPosition'
get_energy(x, expected = FALSE)

# S4 method for class 'GammaSpectrum'
set_names(x) <- value

# S4 method for class 'GammaSpectrum'
set_energy_calibration(x) <- value

# S4 method for class 'GammaSpectrum'
get_energy_calibration(x)

# S4 method for class 'Baseline'
set_method(x) <- value

# S4 method for class 'GammaSpectra'
set_names(x) <- value

# S4 method for class 'GammaSpectra'
set_energy_calibration(x) <- value

# S4 method for class 'GammaSpectra'
get_energy_calibration(x)

# S4 method for class 'PeakPosition,numeric'
set_energy(x) <- value
```

## Arguments

- x:

  An object from which to get or set element(s).

- value:

  A possible value for the element(s) of `x`.

- ...:

  Currently not used.

- na.rm:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should
  [`NA`](https://rdrr.io/r/base/NA.html) be omitted?

- expected:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  expected values be returned instead of observed values?

## Value

An object of the same sort as `x` with the new values assigned.

## See also

Other mutator:
[`subset`](https://crp2a.github.io/gamma/reference/subset.md)

## Author

N. Frerebeau
