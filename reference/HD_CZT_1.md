# heiLUM Calibration Curve (CZT)

heiLUM Calibration Curve (CZT)

## Usage

``` r
data(HD_CZT_1)
```

## Format

Objects of class
[CalibrationCurve](https://crp2a.github.io/gamma/reference/CalibrationCurve-class.md).

|  |  |
|----|----|
| **Laboratory** | heiLUM, Heidelberg University, DE |
| **Instrument** | Kromek GR1 & Kromek RayMon10 (SN: 5261) |
| **Detector** | GR1+ (SN: 1625) & GR1 (SN: 1663) |
| **Authors** | Sebastian Kreutzer, Didier Maillier, Norbert Mercier, heiLUM Team |

The calibration dataset consists of the following objects:

- CAL_GR1_AdamiecAitken1998

- CAL_GR1_Cresswelletal2018

- CAL_GR1_Gurinetal2011

- CAL_GR1_Liritzisetal2013

- CAL_RAYMON_GR1_AdamiecAitken1998

- CAL_RAYMON_GR1_Cresswelletal2018

- CAL_RAYMON_GR1_Gurinetal2011

- CAL_RAYMON_GR1_Liritzisetal2013

## References

Kreutzer, S., Martin, L., Miallier, D. & Mercier, N. (2025).
Environmental gamma dose rate measurements using cadmium zinc telluride
(CZT) detectors. *Geochronology*, 7, p. 229–246.
[doi:10.5194/gchron-7-229-2025](https://doi.org/10.5194/gchron-7-229-2025)

Kreutzer, S., Martin, L., Miallier, D., Mercier, N. & Faske, M. (2024).
Dataset: Environmental Gamma Dose Rate Measurements using CZT Detectors
(1.0.0). *Zenodo*.
[doi:10.5281/zenodo.13731839](https://doi.org/10.5281/zenodo.13731839)

## See also

Other datasets:
[`AIX_NaI_1`](https://crp2a.github.io/gamma/reference/AIX_NaI_1.md),
[`BDX_LaBr_1`](https://crp2a.github.io/gamma/reference/BDX_LaBr_1.md),
[`clermont`](https://crp2a.github.io/gamma/reference/clermont.md),
[`clermont_2024`](https://crp2a.github.io/gamma/reference/clermont_2024.md),
[`heidelberg_2024`](https://crp2a.github.io/gamma/reference/heidelberg_2024.md)

## Examples

``` r
## Load the curve
data(HD_CZT_1, package = "gamma")
names(HD_CZT_1)
#> [1] "CAL_GR1_AdamiecAitken1998"        "CAL_GR1_Gurinetal2011"           
#> [3] "CAL_GR1_Liritzisetal2013"         "CAL_GR1_Cresswelletal2018"       
#> [5] "CAL_RAYMON_GR1_AdamiecAitken1998" "CAL_RAYMON_GR1_Gurinetal2011"    
#> [7] "CAL_RAYMON_GR1_Liritzisetal2013"  "CAL_RAYMON_GR1_Cresswelletal2018"
plot(HD_CZT_1$CAL_GR1_AdamiecAitken1998)

```
