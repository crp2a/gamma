# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrateSignal
#' @aliases integrateSignal,GammaSpectra-method
setMethod(
  f = "integrateSignal",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, peaks = c(238, 1461, 2614.5),
                        noise = NULL, span = NULL, ...) {
    spectra <- as.list(object)
    signals <- lapply(X = spectra, FUN = integrateSignal,
                      peaks = peaks, noise = noise, span = span)
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
                        noise = NULL, span = NULL, ...) {
    # Validation
    # TODO

    # Get spectrum data
    spc_data <- methods::as(object, "data.frame")
    # Remove baseline
    spc_clean <- removeBaseline(object)
    # Detect peaks
    peaks_index <- findPeaks(spc_clean, span = span)
    # Find peaks at 238 keV, 1461 keV and 2614.5 keV
    peaks_select <- sapply(
      X = peaks,
      FUN = function(expected, real) which.min(abs(real - expected)),
      real = peaks_index$energy
    )

    # Get corresponding chanels
    peaks_chanel <- peaks_index$chanel[peaks_select]
    # Fit second order polynomial
    poly_fit <- stats::lm(peaks ~ poly(peaks_chanel, degree = 2, raw = TRUE))
    poly_coef <- stats::coef(poly_fit) %>%
      magrittr::set_names(c("c", "b", "a"))

    # TODO: pas super
    findPolyRoot <- function(a, b, c, delta) {
      (- b + sqrt(b^2 - 4 * (c - delta) * a)) / (2 * a)
    }
    # 200 keV, 2800 keV
    bound_chanel <- sapply(
      X = c(200, 2800),
      FUN = function(x, coef) {
        round(do.call(findPolyRoot, c(as.list(coef), list(delta = x))))
      },
      coef = poly_coef
    )
    seq_chanel <- which(spc_data$chanel >= bound_chanel[1] &
                          spc_data$chanel <= bound_chanel[2])

    # Signal integré
    int_signal <- crossprod(spc_data$energy[seq_chanel],
                            spc_data$counts[seq_chanel])
    int_signal %<>% as.numeric()
    # Signal normalisé au temps
    active_time <- object@live_time
    norm_signal <- int_signal / active_time
    norm_error <- 1 # FIXME

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
