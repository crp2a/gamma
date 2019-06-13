# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
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
      nchar_hash <- nchar(hash)
      if (nchar_hash != 32) {
        message <- c(
          message,
          sprintf(
            "%s must be a 32-character string (not %d) of hexadecimal digits.",
            sQuote("hash"), nchar_hash)
        )
      }
    }
    length_reference <- length(reference)
    if (length_reference > 1) {
      message <- c(
        message,
        sprintf("%s must be character vector of length one, not %d.",
                sQuote("reference"), length_reference)
      )
    }
    length_instrument <- length(instrument)
    if (length_instrument > 1) {
      message <- c(
        message,
        sprintf("%s must be character vector of length one, not %d.",
                sQuote("instrument"), length_instrument)
      )
    }
    length_file_format <- length(file_format)
    if (length_file_format > 1) {
      message <- c(
        message,
        sprintf("%s must be character vector of length one, not %d.",
                sQuote("file_format"), length_file_format)
      )
    }
    length_live_time <- length(live_time)
    if (length_live_time > 1) {
      message <- c(
        message,
        sprintf("%s must be character vector of length one, not %d.",
                sQuote("live_time"), length_live_time)
      )
      if (!isPositive(live_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("live_time"))
        )
      }
    }
    length_real_time <- length(real_time)
    if (length_real_time > 1) {
      message <- c(
        message,
        sprintf(
          "%s must be character vector of length one, not %d.",
          sQuote("real_time"), length_real_time)
      )
      if (!isPositive(real_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("real_time"))
        )
      }
    }
    # TODO: check calibration
    length_dose_rate <- length(dose_rate)
    if (length_dose_rate != 0) {
      if (!isPositive(dose_rate, strict = FALSE)) {
        message <- c(
          message,
          sprintf("%s must be a vector of positive numbers.",
                  sQuote("dose_rate"))
        )
      }
      if (length_dose_rate != 2) {
        message <- c(
          message,
          sprintf("%s must be a numeric vector of length two, not %d.",
                  sQuote("dose_rate"), length_dose_rate)
        )
      }
    }
    if (length(chanel) != 0) {
      if (!all(isPositive(chanel, strict = TRUE))) {
        message <- c(
          message,
          sprintf("%s must be a vector of strictly positive numbers.",
                  sQuote("chanel"))
        )
      }
    }
    # TODO: check counts (baseline may produces negative count)
    # TODO: check rate (baseline may produces negative count)
    if (length(chanel) != 0) {
      if (!isEqual(lengths(list(chanel, count)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("chanel"), length(chanel),
            sQuote("count"), length(count)
          )
        )
      }
    }
    if (length(rate) != 0) {
      if (!isEqual(lengths(list(count, rate)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("count"), length(count),
            sQuote("rate"), length(rate)
          )
        )
      }
    }
    if (length(energy) != 0) {
      if (!isEqual(lengths(list(energy, count)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("energy"), length(energy),
            sQuote("count"), length(count)
          )
        )
      }
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n  * "))
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
      stop("* ", paste0(message, collapse = "\n  * "))
    } else {
      return(TRUE)
    }
  }
)

# CalibrationCurve =============================================================
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
      fields <- c("instrument", "laboratory", "authors", "detector", "date")
      if (!all(fields %in% names(details))) {
        message <- c(
          message,
          sprintf("`details` is a list, but does not have components %s.",
                  paste0(sQuote(fields), collapse = ", "))
        )
      } else {
        instrument <- details$instrument
        laboratory <- details$laboratory
        authors <- details$authors
        detector <- details$detector
        date <- details$date
        if (!is.character(instrument) | length(instrument) > 1) {
          message <- c(
            message,
            sprintf("%s must be a length-one character vector.",
                    sQuote("instrument"))
          )
        }
        if (!is.character(laboratory) | length(laboratory) > 1) {
          message <- c(
            message,
            sprintf("%s must be a length-one character vector.",
                    sQuote("laboratory"))
          )
        }
        if (!is.character(authors)) {
          message <- c(message, sprintf("%s must be a character vector.",
                                        sQuote("authors")))
        }
        if (!is.character(detector) | length(detector) > 1) {
          message <- c(
            message,
            sprintf("%s must be a length-one character vector.",
                    sQuote("detector"))
          )
        }
        if (!methods::is(date, "POSIXct")) {
          message <- c(message, sprintf("%s must be a %s object.",
                                        sQuote("date"), sQuote("POSIXct")))
        }
      }
    }
    length_noise <- length(noise)
    if (length_noise != 0) {
      if (!isPositive(noise, strict = FALSE)) {
        message <- c(
          message,
          sprintf("%s must be a vector of positive numbers.", sQuote("noise"))
        )
      }
      if (length_noise != 2) {
        message <- c(
          message,
          sprintf("%s must be a numeric vector of length two, not %d.",
                  sQuote("noise"), length_noise)
        )
      }
    }
    length_integration <- length(integration)
    if (length_integration != 0) {
      if (!isPositive(integration, strict = FALSE)) {
        message <- c(
          message,
          sprintf("%s must be a vector of positive numbers.",
                  sQuote("integration"))
        )
      }
      if (length_integration != 2) {
        message <- c(
          message,
          sprintf("%s must be a numeric vector of length two, not %d.",
                  sQuote("integration"), length_integration)
        )
      }
    }
    if (length(data) != 0) {
      if (!is.numeric(as.matrix(data[, -1]))) {
        message <- c(message, sprintf("%s must contain numeric values.",
                                      sQuote("data")))
      }
      ncol_data <- ncol(data)
      if (ncol_data != 5) {
        message <- c(
          message,
          sprintf("%s must be a five (not %d) columns data.frame.",
                  sQuote("data"), ncol_data)
        )
      }
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n  * "))
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
        message <- c(message, sprintf("All elements of %s must be of class %s.",
                                      sQuote("model"), sQuote("nls")))
      }
    }
    if (length(coefficients) != 0) {
      if (!is.numeric(coefficients)) {
        message <- c(message, sprintf("%s must be a numeric matrix.",
                                      sQuote("coefficients")))
      }
      col_names <- c("mean", "sd", "height")
      if (!all(colnames(coefficients) %in% col_names) |
          ncol(coefficients) != 3) {
        message <- c(
          message,
          sprintf(
            "%s must be a 3 columns matrix, with column names: %s.",
            sQuote("coefficients"),
            paste(sQuote(col_names), collapse = ", ")
          )
        )
      }
    }
    if (length(spectrum@hash) != 0 & length(baseline@hash) != 0) {
      if (spectrum@hash != baseline@hash)
        message <- c(message, sprintf("%s and %s do not match.",
                                      sQuote("spectrum"), sQuote("baseline")))
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n  * "))
    } else {
      return(TRUE)
    }
  }
)

# PeakPosition =================================================================
setValidity(
  Class = "PeakPosition",
  method = function(object) {
    hash <- object@hash
    noise_method <- object@noise_method
    threshold <- object@noise_threshold
    window <- object@window
    chanel <- object@chanel
    energy <- object@energy
    message <- c()

    length_hash <- length(hash)
    if (length_hash != 1 || (length_hash == 1 && nchar(hash) != 32)) {
        message <- c(message, "Slot `hash` must be a 32-character string.")
    }
    length_method <- length(noise_method)
    if (length_method > 1) {
      message <- c(
        message,
        sprintf("Slot `noise_method` must be a character vector of length 1, not %d.",
                length_method)
      )
    }
    length_noise <- length(threshold)
    if (length_noise > 1) {
      message <- c(
        message,
        sprintf("Slot `noise_threshold` must be a numeric vector of length 1, not %d.",
                length_noise)
      )
    } else if (!is.na(threshold) && !isPositive(threshold, strict = FALSE)) {
      message <- c(message,
                   "Slot `noise_threshold` must be a positive number.")
    }
    length_window <- length(window)
    if (length_window > 1) {
      message <- c(
        message,
        sprintf("Slot `window` must be an integer vector of length 1, not %d.",
                length_window)
      )
    } else if (!is.na(window) && !isPositive(window, strict = TRUE)) {
      message <- c(
        message,
        sprintf("Slot `window` must be a strictly positive integer, not %d.",
                window)
      )
    }
    if (length(chanel) != length(energy)) {
      message <- c(message,
                   "Slots `chanel` and `energy` must have the same length.")
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n* "), call. = FALSE)
    } else {
      return(TRUE)
    }
  }
)
