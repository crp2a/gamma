# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# Helpers ======================================================================
#' Check data input
#'
#' @param x An object to be checked.
#' @param length An \code{\link{integer}} giving the expected length of
#'  \code{x}.
#' @param mode A \code{\link{character}} string giving the expected type of
#'  \code{x}.
#' @return A \code{\link{character}} string (error message) or \code{NULL}.
#' @keywords internal
#' @noRd
checkVector <- function(x, length, mode = c("character", "double", "integer",
                                            "logical", "numeric")) {
  # Validation
  mode <- match.arg(mode, several.ok = FALSE)
  # Get values
  length <- as.integer(length)
  arg <- deparse(substitute(x))
  message <- NULL

  if (!is.vector(x, mode = mode)) {
    message <- sprintf("%s must be a %s vector.", sQuote(arg), mode, length)
  } else {
    n <- length(x)
    if (n != length) {
      message <- sprintf("%s must be a %s vector of length %d not %d.",
                         sQuote(arg), mode, length, n)
    }
  }

  return(message)
}

# GammaSpectrum ================================================================
# TODO: check .@date
# TODO: check .@calibration
setValidity(
  Class = "GammaSpectrum",
  method = function(object) {
    hash <- object@hash
    reference <- object@reference
    date <- object@date
    instrument <- object@instrument
    file_format <- object@file_format
    chanel <- object@chanel
    energy <- object@energy
    count <- object@counts
    rate <- object@rate
    live_time <- object@live_time
    real_time <- object@real_time
    calibration <- object@calibration
    dose_rate <- object@dose_rate
    message <- c()

    length_hash <- length(hash)
    if (length_hash != 0) {
      message <- c(message, checkVector(hash, 1, "character"))
      if (nchar(hash) != 32) {
        message <- c(
          message,
          sprintf("%s must be a 32-character string of hexadecimal digits.",
                  sQuote("hash"))
        )
      }
    }
    if (length(reference) != 0) {
      message <- c(message, checkVector(reference, 1, "character"))
    }
    if (length(instrument) != 0) {
      message <- c(message, checkVector(instrument, 1, "character"))
    }
    if (length(file_format) != 0) {
      message <- c(message, checkVector(file_format, 1, "character"))
    }
    if (length(live_time) != 0) {
      message <- c(message, checkVector(live_time, 1, "numeric"))
      if (!isPositive(live_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("live_time"))
        )
      }
    }
    if (length(real_time) != 0) {
      message <- c(message, checkVector(real_time, 1, "numeric"))
      if (!isPositive(real_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("real_time"))
        )
      }
    }
    if (length(chanel) != 0) {
      if (!all(isPositive(chanel, strict = TRUE))) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("chanel"))
        )
      }
    }
    # FIXME: baseline may produces negative count
    # if (length(count) != 0) {
    #   if (!all(isPositive(count, strict = FALSE))) {
    #     message <- c(
    #       message,
    #       sprintf("%s must be a positive number.", sQuote("count"))
    #     )
    #   }
    # }
    x <- lengths(list(chanel, count))
    if (!isEqual(x)) {
      message <- c(
        message,
        sprintf("%s (%d) and %s (%d) must have the same length.",
                sQuote("chanel"), length(chanel),
                sQuote("count"), length(count))
      )
    }
    if (length(energy) != 0) {
      # if (!all(isPositive(energy, strict = TRUE))) {
      #   message <- c(
      #     message,
      #     sprintf("%s must be a strictly positive number.", sQuote("energy"))
      #   )
      # }
      y <- lengths(list(chanel, count, energy))
      if (!isEqual(y)) {
        message <- c(
          message,
          sprintf("%s (%d), %s (%d) and %s (%d) must have the same length.",
                  sQuote("chanel"), length(chanel),
                  sQuote("count"), length(count),
                  sQuote("energy"), length(energy))
        )
      }
    }
    if (length(dose_rate) != 0) {
      if (!isPositive(dose_rate, strict = FALSE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("dose_rate"))
        )
      }
      if (length(dose_rate) != 2) {
        message <- c(
          message,
          sprintf("%s must be a length-two numeric vector.", sQuote("dose_rate"))
        )
      }
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# GammaSpectra =================================================================
setValidity(
  Class = "GammaSpectra",
  method = function(object) {
    data <- object@.Data
    message <- c()

    if (length(data) != 0) {
      data_class <- unlist(lapply(X = data, FUN = is, class2 = "GammaSpectrum"))
      if (!all(data_class)) {
        message <- c(
          message,
          sprintf("All elements must be of class %s.", sQuote("GammaSpectrum"))
        )
      }
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# CalibrationCurve =============================================================
# TODO: check @date
# TODO: check @model
setValidity(
  Class = "CalibrationCurve",
  method = function(object) {
    details <- object@details
    model <- object@model
    noise <- object@noise
    integration <- object@integration
    data <- object@data
    message <- c()

    if (length(details) != 0) {
      instrument <- details$instrument
      laboratory <- details$laboratory
      authors <- details$authors
      detector <- details$detector
      date <- details$date
      if (length(instrument) != 0) {
        message <- c(message, checkVector(instrument, 1, "character"))
      }
      if (length(laboratory) != 0) {
        message <- c(message, checkVector(laboratory, 1, "character"))
      }
      if (length(detector) != 0) {
        message <- c(message, checkVector(detector, 1, "character"))
      }
      if (length(authors) != 0) {
        message <- c(message, checkVector(authors, 1, "character"))
      }
      if (length(date) != 0) {
        if (!methods::is(date, "POSIXct"))
          message <- c(message, sprintf("%s must be a %s object.",
                                        sQuote("date"), sQuote("POSIXct")))
      }
    }
    if (length(noise) != 0) {
      message <- c(message, checkVector(noise, 2, "numeric"))
    }
    if (length(integration) != 0) {
      message <- c(message, checkVector(integration, 2, "numeric"))
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# DoseRate =====================================================================
setValidity(
  Class = "DoseRate",
  method = function(object) {
    reference <- object@reference
    dose_value <- object@dose_value
    dose_error <- object@dose_error
    signal_value <- object@signal_value
    signal_error <- object@signal_error
    message <- c()

    n_reference <- length(reference)
    n_dose_value <- length(dose_value)
    n_dose_error <- length(dose_error)
    n_signal_value <- length(signal_value)
    n_signal_error <- length(signal_error)
    if (length(reference) != 0) {
      if (anyNA(reference)) {
        message <- c(
          message,
          sprintf("Missing values were detected in %s.", sQuote("reference"))
        )
      }
    }
    if (n_dose_value != 0) {
      if (!isEqual(n_reference, n_dose_value)) {
        message <- c(
          message,
          sprintf("%s must be of length %d, not %d.",
                  sQuote("dose_value"), n_reference, n_dose_value)
        )
      }
      if (!all(is.finite(dose_value))) {
        message <- c(
          message,
          sprintf("Missing or infinite values were detected in %s.",
                  sQuote("dose_value"))
        )
      }
    }
    if (n_dose_error != 0) {
      if (!isEqual(n_reference, n_dose_error)) {
        message <- c(
          message,
          sprintf("%s must be of length %d, not %d.",
                  sQuote("dose_value"), n_reference, n_dose_error)
        )
      }
      if (!all(is.finite(dose_error))) {
        message <- c(
          message,
          sprintf("Missing or infinite values were detected in %s.",
                  sQuote("dose_error"))
        )
      }
    }
    if (n_signal_value != 0) {
      if (!isEqual(n_reference, n_signal_value)) {
        message <- c(
          message,
          sprintf("%s must be of length %d, not %d.",
                  sQuote("dose_value"), n_reference, n_signal_value)
        )
      }
      if (!all(is.finite(signal_value))) {
        message <- c(
          message,
          sprintf("Missing or infinite values were detected in %s.",
                  sQuote("signal_value"))
        )
      }
    }
    if (n_signal_error != 0) {
      if (!isEqual(n_reference, n_signal_error)) {
        message <- c(
          message,
          sprintf("%s must be of length %d, not %d.",
                  sQuote("dose_value"), n_reference, n_signal_error)
        )
      }
      if (!all(is.finite(signal_error))) {
        message <- c(
          message,
          sprintf("Missing or infinite values were detected in %s.",
                  sQuote("signal_error"))
        )
      }
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# PeakModel ====================================================================
setValidity(
  Class = "PeakModel",
  method = function(object) {
    model <- object@model
    coefficients <- object@coefficients
    spectrum <- object@spectrum
    baseline <- object@baseline
    message <- c()

    if (length(model) != 0) {
      model_class <- unlist(lapply(X = model, FUN = is, class2 = "nls"))
      if (!all(model_class)) {
        message <- c(
          message,
          sprintf("All elements of %s must be of class %s.",
                  sQuote("model"), sQuote("nls"))
        )
      }
    }
    if (length(coefficients) != 0) {
      if (!is.numeric(coefficients)) {
        message <- c(
          message,
          sprintf("%s must be a numeric matrix.", sQuote("coefficients"))
        )
      }
      col_names <- c("mean", "sd", "height")
      if (!all(col_names %in% colnames(coefficients))) {
        message <- c(
          message,
          sprintf("%s must be a 3 columns matrix, with column names: %s.",
                  sQuote("coefficients"), paste(sQuote(col_names), collapse = ", "))
        )
      }
    }
    if (length(spectrum@hash) != 0 & length(baseline@hash) != 0) {
      if (spectrum@hash != baseline@hash)
        message <- c(
          message,
          sprintf("%s and %s do not match.",
                  sQuote("spectrum"), sQuote("baseline"))
        )
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

# PeakPosition =================================================================
setValidity(
  Class = "PeakPosition",
  method = function(object) {
    method <- object@method
    noise <- object@noise
    window <- object@window
    peaks <- object@peaks
    spectrum <- object@spectrum
    baseline <- object@baseline
    message <- c()

    if (length(method) != 0) {
      message <- c(message, checkVector(method, 1, "character"))
    }
    if (length(noise) != 0) {
      message <- c(message, checkVector(noise, 1, "numeric"))
      if (!all(isPositive(noise, strict = FALSE))) {
        message <- c(
          message,
          sprintf("%s must be a positive number.", sQuote("noise"))
        )
      }
    }
    if (length(window) != 0) {
      message <- c(message, checkVector(window, 1, "numeric"))
      if (!all(isPositive(window, strict = FALSE))) {
        message <- c(
          message,
          sprintf("%s must be a positive number.", sQuote("window"))
        )
      }
    }
    if (length(peaks) != 0) {
      col_names <- c("chanel", "energy", "counts", "rate")
      if (!all(col_names %in% colnames(peaks))) {
        message <- c(
          message,
          sprintf("%s must be a 4 columns data frame, with column names: %s",
                  sQuote("peaks"), paste(sQuote(col_names), collapse = ", "))
        )
      }
    }
    if (length(spectrum@hash) != 0 & length(baseline@hash) != 0) {
      if (spectrum@hash != baseline@hash) {
        message <- c(
          message,
          sprintf("%s and %s do not match.",
                  sQuote("spectrum"), sQuote("baseline"))
        )
      }
    }

    if (length(message) != 0) {
      stop(paste(message, collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)
