# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "missing"),
  definition = function(x, xaxis = c("energy", "chanel"),
                        yaxis = c("counts", "rate"), ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)

    spc <- methods::as(x, "data.frame")

    ggplot2::ggplot(spc, ggplot2::aes_string(x = xaxis, y = yaxis)) +
      ggplot2::geom_line()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,BaseLine-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "BaseLine"),
  definition = function(x, y, xaxis = c("energy", "chanel"),
                        yaxis = c("counts", "rate"), ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)

    spc <- methods::as(x, "data.frame")
    bl <- methods::as(y, "data.frame")

    # Bind data frame for ggplot2
    # Reverse order of factor to display the baseline in front of the spectrum
    data <- dplyr::bind_rows("spectrum" = spc, "baseline" = bl, .id = "curve") %>%
      dplyr::mutate(curve = factor(.data$curve, levels = c("spectrum", "baseline")))

    ggplot2::ggplot(data, ggplot2::aes_string(x = xaxis, y = yaxis,
                                              group = "curve", colour = "curve")) +
      ggplot2::geom_line()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectra,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectra", y = "missing"),
  definition = function(x, xaxis = c("energy", "chanel"),
                        yaxis = c("counts", "rate"),
                        select = 1, facet = FALSE, ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)
    if (is.null(select))
      select <- 1:length(x)
    if (is.numeric(select))
      select <- as.integer(select)

    # Subset data
    spc <- methods::as(x[select], "list")
    if (length(spc) == 1)
      facet <- FALSE

    # Build long data frame
    spc_ls <- lapply(X = spc, FUN = "as", Class = "data.frame")
    spc_df <- dplyr::bind_rows(spc_ls, .id = "reference")

    if (facet) {
      colour <- NULL
      facet <- ggplot2::facet_wrap(.~reference, nrow = length(spc_ls),
                                   scales = "free_y")
    } else {
      colour <- "reference"
      facet <- NULL
    }
    ggplot2::ggplot(data = spc_df,
                    mapping = ggplot2::aes_string(
                      x = xaxis, y = yaxis,
                      group = "reference", colour = colour)
                    ) +
      ggplot2::geom_line() +
      facet
  }
)

#' @export
#' @rdname plot
#' @aliases plot,PeakPosition,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "PeakPosition", y = "missing"),
  definition = function(x, xaxis = c("energy", "chanel"),
                        yaxis = c("counts", "rate"), ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)
    # Get data
    spc <- x@spectrum
    peaks <- x@peaks

    plot(spc, xaxis = xaxis, yaxis = yaxis) +
      ggplot2::geom_vline(xintercept = peaks[, xaxis], linetype = 3,
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
    spc <- x@spectrum
    spc_df <- methods::as(spc, "data.frame")
    fit <- stats::predict(x@model, spc_df$energy)

    plot(spc, xaxis = "energy", yaxis = "counts") +
      ggplot2::geom_area(mapping = ggplot2::aes_string(y = "fit"),
                         fill = "blue", colour = "blue", alpha = 0.5)
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
    data <- methods::as(x, "data.frame")

    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes_string(x = "dose", y = "signal",
                                                  label = "reference")) +
      ggplot2::geom_point() +
      ggplot2::stat_smooth(method = "lm", col = "red",
                           formula = y ~ 0 + x, se = FALSE)
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,DoseRate-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "DoseRate"),
  definition = function(x, y, ...) {
    # Get data
    calib <- methods::as(x, "data.frame")
    measure <- methods::as(y, "data.frame")

    # Bind data frame for 'ggplot2'
    data <- dplyr::bind_rows(calib, measure) %>%
      dplyr::mutate(spectrum = c(rep("calibration", nrow(calib)),
                                 rep("measured", nrow(measure))))

    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes_string(x = "dose", y = "signal",
                                                  colour = "spectrum")) +
      ggplot2::stat_smooth(data = subset(data, data$spectrum == "calibration"),
                           method = "lm", col = "black",
                           formula = y ~ 0 + x, se = FALSE) +
      ggplot2::geom_point()
  }
)
