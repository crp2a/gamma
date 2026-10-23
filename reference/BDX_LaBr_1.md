# CRP2A Calibration Curve (LaBr, probe 1)

CRP2A Calibration Curve (LaBr, probe 1)

## Usage

``` r
data(BDX_LaBr_1)
```

## Format

An object of class
[CalibrationCurve](https://crp2a.github.io/gamma/reference/CalibrationCurve-class.md).

|                |                         |
|----------------|-------------------------|
| **Laboratory** | IRAMAT-CRP2A (UMR 5060) |
| **Instrument** | Canberra Inspector 1000 |
| **Detector**   | LaBr                    |
| **Authors**    | CRP2A Luminescence Team |

## See also

Other datasets:
[`AIX_NaI_1`](https://crp2a.github.io/gamma/reference/AIX_NaI_1.md),
[`HD_CZT_1`](https://crp2a.github.io/gamma/reference/HD_CZT_1.md),
[`clermont`](https://crp2a.github.io/gamma/reference/clermont.md),
[`clermont_2024`](https://crp2a.github.io/gamma/reference/clermont_2024.md),
[`heidelberg_2024`](https://crp2a.github.io/gamma/reference/heidelberg_2024.md)

## Examples

``` r
## Load the curve
data(BDX_LaBr_1, package = "gamma")
plot(BDX_LaBr_1)
```
