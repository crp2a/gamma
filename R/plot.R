# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "missing"),
  definition = function(x, ...) {
    # Get data
    spc <- methods::as(x, "data.frame")
    # Secondary axes
    sec_xaxis <- ggplot2::scale_x_continuous(name = "Chanel")
    sec_yaxis <- ggplot2::scale_y_continuous(name = "Count")
    if (!anyNA(spc$energy)) {
      sec_xaxis <- ggplot2::scale_x_continuous(
        name = "Chanel",
        sec.axis = ggplot2::sec_axis(~ spc$energy, name = "Energy [keV]")
      )
    }
    live_time <- x[["live_time"]]
    if (length(live_time) != 0) {
      sec_yaxis <- ggplot2::scale_y_continuous(
        name = "Count",
        sec.axis = ggplot2::sec_axis(~ . / live_time, name = "Count rate [1/s]")
      )
    }
    # Plot
    ggplot2::ggplot(spc, ggplot2::aes_string(x = "chanel", y = "counts")) +
      sec_xaxis + sec_yaxis +
      ggplot2::geom_line()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "GammaSpectrum"),
  definition = function(x, y, xaxis = c("chanel", "energy"),
                        yaxis = c("counts", "rate"), ...) {
    spc <- methods::new("GammaSpectra", list(x, y))
    plot(spc, xaxis = xaxis, yaxis = yaxis, select = NULL, facet = FALSE)
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectra,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectra", y = "missing"),
  definition = function(x, xaxis = c("chanel", "energy"),
                        yaxis = c("counts", "rate"),
                        select = NULL, facet = FALSE, ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)
    if (is.null(select))
      select <- 1:length(x)
    if (is.numeric(select))
      select <- as.integer(select)

    # Subset data and build a long data frame
    spc <- methods::as(x[select], "data.frame")
    n <- nlevels(as.factor(spc$reference))

    if (xaxis == "energy" & anyNA(spc$energy)) {
      xaxis <- "chanel"
      warning(paste("The energy scale is missing for one or more spectra",
                    "using the chanel scale instead.", sep = ", "))
    }
    xlabel <- switch(xaxis, chanel = "Chanel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, counts = "Counts", rate = "Count rate [1/s]")

    facet <- if (n == 1) FALSE else facet
    if (facet) {
      colour <- NULL
      facet <- ggplot2::facet_wrap(. ~ reference, nrow = n, scales = "free_y")
    } else {
      colour <- "reference"
      facet <- NULL
    }
    ggplot2::ggplot(
      data = spc,
      mapping = ggplot2::aes_string(
        x = xaxis, y = yaxis,
        group = "reference", colour = colour)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = xlabel) +
      ggplot2::scale_y_continuous(name = ylabel) +
      facet
  }
)

#' @export
#' @rdname plot
#' @aliases plot,PeakPosition,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "PeakPosition", y = "missing"),
  definition = function(x, ...) {
    # Get data
    spc <- x@spectrum
    peaks <- x@peaks

    plot(spc) +
      ggplot2::geom_vline(xintercept = peaks[, "chanel"], linetype = 3,
                          colour = "red")
  }
)

#' @export
#' @rdname plot
#' @aliases plot,PeakModel,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "PeakModel", y = "missing"),
  definition = function(x, ...) {
    # Get data
    bl <- x@baseline
    spc <- x@spectrum

    spc_clean <- spc - bl
    spc_df <- methods::as(spc_clean, "data.frame")
    fit <- lapply(
      X = x@model,
      FUN = function(x, data) stats::predict(x, data),
      data = spc_df[, "chanel"]
    )

    # Build long table for ggplot2
    spc_long <- do.call(cbind, fit) %>%
      as.data.frame() %>%
      stats::setNames(paste("peak", 1:ncol(.), sep = " ")) %>%
      dplyr::bind_cols(spc_df) %>%
      tidyr::gather(key = "peak", value = "fit",
                    -.data$chanel, -.data$energy, -.data$counts, -.data$rate)

    plot(spc_clean) +
      ggplot2::geom_area(
        data = spc_long,
        mapping = ggplot2::aes_string(x = "chanel", y = "fit",
                                      fill = "peak", colour = "peak"),
        alpha = 0.5
      )
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "missing"),
  definition = function(x, ...) {
    # Get data
    data <- methods::as(x@data, "data.frame")
    signal <- range(data$signal_value)

    # Set error bar width and height
    error_width <- sum(signal * c(-1, 1)) / 100
    error_height <- sum(range(data$dose_value) * c(-1, 1)) / 100

    # Curve
    curve <- data.frame(signal_value = signal) %>%
      stats::predict.lm(x@model, .) %>%
      c(signal, .) %>%
      magrittr::set_names(c("x", "xmin", "y", "ymin")) %>%
      as.matrix() %>%
      t() %>%
      as.data.frame()

    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes_string(
        x = "signal_value", y = "dose_value",
        label = "reference")) +
      ggplot2::geom_segment(
        data = curve,
        mapping = ggplot2::aes_string(
          x = "x", xend = "xmin",
          y = "y", yend = "ymin"),
        colour = "red",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes_string(
          ymin = "dose_value - dose_error",
          ymax = "dose_value + dose_error"),
        width = error_width) +
      ggplot2::geom_errorbarh(
        mapping = ggplot2::aes_string(
          xmin = "signal_value - signal_error",
          xmax = "signal_value + signal_error"),
        height = error_height) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(name = "Signal") +
      ggplot2::scale_y_continuous(name = "Dose rate [\u03BCGy/y]")
  }
)

# TODO
# @export
# @rdname plot
# @aliases plot,CalibrationCurve,GammaSpectra-method
# setMethod(
#   f = "plot",
#   signature = signature(x = "CalibrationCurve", y = "GammaSpectra"),
#   definition = function(x, y, ...) {
#     # Get data
#     calib <- methods::as(x@data, "data.frame")
#     measure <- getDoseRate(y)
#
#     # Bind data frame for 'ggplot2'
#     data <- dplyr::bind_rows(calib, measure) %>%
#       dplyr::mutate(spectrum = c(rep("calibration", nrow(calib)),
#                                  rep("estimate", nrow(measure))))
#     # Set error bar width and height
#     error_width <- sum(range(data$signal_value) * c(-1, 1)) / 100
#     error_height <- sum(range(data$dose_value) * c(-1, 1)) / 100
#
#     # Curve
#     calib_signal <- range(calib$signal_value)
#     curve <- data.frame(signal_value = calib_signal) %>%
#       stats::predict.lm(x@model, .) %>%
#       c(calib_signal, .) %>%
#       magrittr::set_names(c("x", "xmin", "y", "ymin")) %>%
#       as.matrix() %>%
#       t() %>%
#       as.data.frame()
#
#     ggplot2::ggplot(
#       data = data,
#       mapping = ggplot2::aes_string(
#         x = "signal_value", y = "dose_value",
#         colour = "spectrum", label = "reference")) +
#       ggplot2::geom_segment(
#         data = curve,
#         mapping = ggplot2::aes_string(
#           x = "x", xend = "xmin",
#           y = "y", yend = "ymin"),
#         colour = "black",
#         inherit.aes = FALSE) +
#       ggplot2::geom_errorbar(
#         mapping = ggplot2::aes_string(
#           ymin = "dose_value - dose_error",
#           ymax = "dose_value + dose_error"),
#         width = error_width) +
#       ggplot2::geom_errorbarh(
#         mapping = ggplot2::aes_string(
#           xmin = "signal_value - signal_error",
#           xmax = "signal_value + signal_error"),
#         height = error_height) +
#       ggplot2::geom_point() +
#       ggplot2::scale_x_continuous(name = "Signal") +
#       ggplot2::scale_y_continuous(name = "Dose rate [\u03BCGy/y]")
#   }
# )
