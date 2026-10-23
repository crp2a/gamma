# CEREGE Calibration Curve (NaI)

CEREGE Calibration Curve (NaI)

## Usage

``` r
data(AIX_NaI_1)
```

## Format

An object of class
[CalibrationCurve](https://crp2a.github.io/gamma/reference/CalibrationCurve-class.md).

|                |                          |
|----------------|--------------------------|
| **Laboratory** | CEREGE                   |
| **Instrument** | Canberra Inspector 1000  |
| **Detector**   | NaI                      |
| **Authors**    | CEREGE Luminescence Team |

## See also

Other datasets:
[`BDX_LaBr_1`](https://crp2a.github.io/gamma/reference/BDX_LaBr_1.md),
[`HD_CZT_1`](https://crp2a.github.io/gamma/reference/HD_CZT_1.md),
[`clermont`](https://crp2a.github.io/gamma/reference/clermont.md),
[`clermont_2024`](https://crp2a.github.io/gamma/reference/clermont_2024.md),
[`heidelberg_2024`](https://crp2a.github.io/gamma/reference/heidelberg_2024.md)

## Examples

``` r
## Load the curve
data(AIX_NaI_1, package = "gamma")
plot(AIX_NaI_1)
```
