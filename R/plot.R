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

    sec_xaxis <- sec_yaxis <- ggplot2::waiver()
    if (!anyNA(spc[["chanel"]])) {
      xaxis <- "chanel"
      if (!anyNA(spc[["energy"]])) {
        sec_xaxis <- ggplot2::sec_axis(~ spc[["energy"]], name = "Energy [keV]")
      }
    } else if (!anyNA(spc[["energy"]])) {
      xaxis <- "energy"
    } else {
      stop("Nothing to plot!", call. = FALSE)
    }

    live_time <- x[["live_time"]]
    if (length(live_time) != 0) {
      sec_yaxis <- ggplot2::sec_axis(~ . / live_time, name = "Count rate [1/s]")
    }

    # Plot
    ggplot2::ggplot(spc, ggplot2::aes(x = .data[[xaxis]], y = .data$counts)) +
      ggplot2::scale_x_continuous(sec.axis = sec_xaxis) +
      ggplot2::scale_y_continuous(sec.axis = sec_yaxis) +
      ggplot2::labs(x = xaxis, y = "Count") +
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
    spc <- .GammaSpectra(list(x, y))
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
      select <- seq_len(length(x))
    if (is.numeric(select))
      select <- as.integer(select)

    # Subset data and build a long data frame
    spc <- methods::as(x[select], "data.frame")
    n <- nlevels(as.factor(spc$reference))

    if (xaxis == "energy" & anyNA(spc$energy)) {
      xaxis <- "chanel"
      warning("The energy scale is missing for one or more spectra, ",
              "using the chanel scale instead.", call. = FALSE)
    }
    xlabel <- switch(xaxis, chanel = "Chanel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, counts = "Counts", rate = "Count rate [1/s]")

    facet <- if (n == 1) FALSE else facet
    if (facet) {
      facet <- ggplot2::facet_wrap(ggplot2::vars(.data$reference),
                                   nrow = n, scales = "free_y")
      aes_plot <- ggplot2::aes(x = .data[[xaxis]], y = .data[[yaxis]],
                               group = .data$reference)
    } else {
      facet <- NULL
      aes_plot <- ggplot2::aes(x = .data[[xaxis]], y = .data[[yaxis]],
                               group = .data$reference,
                               colour = .data$reference)
    }
    ggplot2::ggplot(
      data = spc,
      mapping = aes_plot) +
      ggplot2::geom_line() +
      ggplot2::labs(x = xlabel, y = ylabel, colour = "Reference") +
      facet
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,PeakPosition-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "PeakPosition"),
  definition = function(x, y, ...) {
    # Validation
    if (x@hash != y@hash)
      stop("`x` and `y` do not match.", call. = FALSE)

    # Get data
    peak_chanel <- y@chanel
    peak_energy <- y@energy
    index_energy <- !is.na(peak_energy)
    legend <- NULL
    if (any(index_energy)) {
      legend <- ggplot2::annotate(
        "text",
        x = peak_chanel[index_energy],
        y = max(x@counts) * 0.8,
        label = paste0(round(peak_energy[index_energy], 1), " keV"),
        angle = 90, hjust = 0, vjust = 1.5
      )
    }

    plot(x) +
      ggplot2::geom_vline(
        xintercept = peak_chanel,
        linetype = 3,
        colour = "red"
      ) +
      legend
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
      stats::setNames(paste("peak", seq_len(ncol(.)), sep = " ")) %>%
      dplyr::bind_cols(spc_df) %>%
      tidyr::gather(key = "peak", value = "fit",
                    -.data$chanel, -.data$energy, -.data$counts, -.data$rate)

    plot(spc_clean) +
      ggplot2::geom_area(
        data = spc_long,
        mapping = ggplot2::aes(x = .data$chanel, y = .data$fit,
                               fill = .data$peak, colour = .data$peak),
        alpha = 0.5
      ) +
      ggplot2::labs(colour = "Peak", fill = "Peak")
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
      mapping = ggplot2::aes(x = .data$signal_value, y = .data$dose_value,
                             label = .data$reference)) +
      ggplot2::geom_segment(
        data = curve,
        mapping = ggplot2::aes(x = .data$x, xend = .data$xmin,
                               y = .data$y, yend = .data$ymin),
        colour = "red",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(ymin = .data$dose_value - .data$dose_error,
                               ymax = .data$dose_value + .data$dose_error),
        width = error_width) +
      ggplot2::geom_errorbarh(
        mapping = ggplot2::aes(xmin = .data$signal_value - .data$signal_error,
                               xmax = .data$signal_value + .data$signal_error),
        height = error_height) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Signal", y = "Dose rate [\u03BCGy/y]")
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
