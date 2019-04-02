# BUILD CALIBRATION CURVE AND ADJUST EXPERIMENTAL DATA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname adjust
#' @aliases adjust,GammaSpectra-method
setMethod(
  f = "adjust",
  signature = signature(object = "GammaSpectra", curve = "CalibrationCurve"),
  definition = function(object, curve, noise = NULL, ...) {

    signals <- integrateSignal(object, noise = noise, ...) %>%
      dplyr::bind_rows(.id = "reference") %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Get linear regression results
    fit <- curve@model
    slope_value <- stats::coef(fit)
    slope_error <- summary(curve@model)$coef[, "Std. Error"]

    # @param x a data.frame
    # @param slope
    # @param error
    # @return A list
    calcDoseRate <- function(x, slope, error, calibration = 3) {
      signal_value <- as.numeric(x["signal"])
      signal_error <- as.numeric(x["signal_error"])
      dose_rate <- signal_value / slope
      dose_error <- sqrt((100 * signal_error / signal_value)^2 +
                           error^2 + calibration^2) * dose_rate / 100
      return(list(dose = dose_rate, error = dose_error))
    }

    final <- apply(X = signals, MARGIN = 1, FUN = calcDoseRate,
                   slope = slope_value, error = slope_error) %>%
      magrittr::set_names(names(object)) %>%
      dplyr::bind_rows(.id = "reference")

    methods::new("DoseRate",
                 reference = signals$reference,
                 dose_value = final$dose,
                 dose_error = final$error,
                 signal_value = signals$signal,
                 signal_error = signals$signal_error)
  }
)

#' @export
#' @rdname calibrate
#' @aliases calibrate,GammaSpectra-method
setMethod(
  f = "calibrate",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, dose, noise = NULL, ...) {

    signals <- integrateSignal(object, noise = noise, ...) %>%
      dplyr::bind_rows(.id = "reference") %>%
      dplyr::rename(signal = "value", signal_error = "error")

    # Check names
    if (sum(signals$reference %in% names(dose)) != nrow(signals))
      stop("XXX")

    # Fit linear regression
    data <- dose %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      dplyr::mutate(reference = rownames(.)) %>%
      dplyr::rename(dose = "V1", dose_error = "V2") %>%
      dplyr::inner_join(signals, by = "reference") %>%
      dplyr::select(.data$reference, .data$dose, .data$dose_error,
                    .data$signal, .data$signal_error)

    fit <- stats::lm(signal ~ 0 + dose, data = data)

    methods::new("CalibrationCurve", model = fit, data = data)
  }
)

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectra-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, peaks = c(238, 1461, 2614.5),
                        noise = NULL, m = 3, ...) {
    spectra <- as.list(object)
    signals <- lapply(X = spectra, FUN = integrateSignal,
                      peaks = peaks, noise = noise, m = m)
    return(signals)
  }
)

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectrum-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, peaks = c(238, 1461, 2614.5),
                        noise = NULL, m = 3, ...) {
    # Validation
    m <- as.integer(m)

    # Get data
    spc <- methods::as(object, "data.frame")
    k <- nrow(spc)
    # Cut data, starting at maximum value
    spc_max <- which.max(spc$counts)
    spc_cut <- spc[spc_max:k, ]

    # Fit non-linear regression model
    # (asymptotic regression model)
    nls_formula <- stats::formula(counts ~ SSasymp(energy, Asym, R0, lrc))
    # nls_init <- stats::getInitial(nls_formula, data = spc_cut)
    nls_fit <- stats::nls(nls_formula, data = spc_cut) %>%
      broom::augment()

    # Find peaks at 238 keV, 1461 keV and 2614.5 keV
    peaks_index <- findPeaks(nls_fit$.resid, m = m)
    peaks_energy <- nls_fit$energy[peaks_index]

    energy <- sapply(
      X = peaks,
      FUN = function(expected, real) real[which.min(abs(real - expected))],
      real = peaks_energy
    )

    # Find corresponding chanels
    chanels <- spc$chanel[which(spc$energy %in% energy)]
    # Fit second order polynomial
    poly_fit <- stats::lm(peaks ~ poly(chanels, degree = 2, raw = TRUE))
    poly_coef <- stats::coef(poly_fit) %>%
      magrittr::set_names(c("c", "b", "a"))

    # 200 keV, 2800 keV
    bound_chanel <- sapply(
      X = c(200, 2800),
      FUN = function(x, coef) {
        findPolyRoot <- function(a, b, c, delta) {
          (- b + sqrt(b^2 - 4 * (c - delta) * a)) / (2 * a)
        }
        round(do.call(findPolyRoot, c(as.list(coef), list(delta = x))))
      },
      coef = poly_coef
    )
    seq_chanel <- which(spc$chanel >= bound_chanel[1] &
                          spc$chanel <= bound_chanel[2])

    # Signal integré
    int_signal <- crossprod(spc$energy[seq_chanel], spc$counts[seq_chanel]) %>%
      as.numeric()
    # Signal normalisé au temps
    active_time <- object@live_time
    norm_signal <- int_signal / active_time
    norm_error <- 0 # FIXME

    if (!is.null(noise)) {
      # Validation
      if (length(noise) != 2)
        stop("YYY")
      if (!all.equal(lengths(noise), c(value = 1, error = 1)))
        stop("ZZZ")

      # Signal normalisé net (bruit de fond soustrait)
      net_signal <- norm_signal - noise$value
      net_error <- sqrt((sqrt(2 * int_signal) / active_time)^2 + noise$error^2)

      return(list(value = net_signal, error = net_error))
    } else {
      # return(list(nls = nls_fit, poly = poly_fit, peaks = energy))
      return(list(value = norm_signal, error = norm_error))
    }
  }
)
