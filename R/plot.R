# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "missing"),
  definition = function(x, xaxis = c("chanel", "energy"),
                        yaxis = c("count", "rate"), ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)

    # Get data
    calib <- is_calibrated(x)
    spc <- methods::as(x, "data.frame")
    if (xaxis == "energy" && anyNA(spc[["energy"]])) {
      xaxis <- "chanel"
      warning("The energy scale is missing, displaying chanels instead.",
              call. = FALSE)
    }
    xlabel <- switch(xaxis, chanel = "Chanel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, count = "Counts", rate = "Count rate [1/s]")

    # Plot
    ggplot(spc, aes(x = .data[[xaxis]], y = .data[[yaxis]])) +
      labs(x = xlabel, y = ylabel) +
      geom_path()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "GammaSpectrum"),
  definition = function(x, y, xaxis = c("chanel", "energy"),
                        yaxis = c("count", "rate"), ...) {
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
                        yaxis = c("count", "rate"),
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
    n <- nlevels(as.factor(spc$name))

    if (xaxis == "energy" & anyNA(spc$energy)) {
      xaxis <- "chanel"
      warning("The energy scale is missing for one or more spectra, ",
              "displaying chanels instead.", call. = FALSE)
    }
    xlabel <- switch(xaxis, chanel = "Chanel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, count = "Counts", rate = "Count rate [1/s]")

    facet <- if (n == 1) FALSE else facet
    if (facet) {
      facet <- facet_wrap(vars(.data$name), nrow = n, scales = "free_y")
      aes_plot <- aes(x = .data[[xaxis]], y = .data[[yaxis]],
                      group = .data$name)
    } else {
      facet <- NULL
      aes_plot <- aes(x = .data[[xaxis]], y = .data[[yaxis]],
                      group = .data$name,
                      colour = .data$name)
    }
    ggplot(data = spc, mapping = aes_plot) +
      geom_path() +
      labs(x = xlabel, y = ylabel, colour = "Name") +
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
    peak_chanel <- y[["chanel"]]
    peak_energy <- y[["energy"]]

    index_energy <- !is.na(peak_energy)
    if (any(index_energy)) {
      sec_axis <- sec_axis(
        trans = ~.,
        name = "Energy [keV]",
        breaks = peak_chanel[index_energy],
        labels = round(peak_energy[index_energy], 0)
      )
    } else {
      sec_axis <- waiver()
    }
    peak_legend <- scale_x_continuous(
      breaks = peak_chanel,
      labels = peak_chanel,
      sec.axis = sec_axis
    )
    plot(x) +
      geom_vline(xintercept = peak_chanel, linetype = 3, colour = "red") +
      peak_legend
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "missing"),
  definition = function(x, threshold = c("Ni", "NiEi"), ...) {
    # Validation
    threshold <- match.arg(threshold, several.ok = FALSE)

    # Get data
    data <- x[["data"]]
    model <- get_model(x, threshold)
    signal_value <- paste0(threshold, "_signal")
    signal_error <- paste0(threshold, "_error")
    signal <- range(data[[signal_value]])

    # Curve
    segment_x <- range(data[[signal_value]])
    segment_y <- model[1, 1] * segment_x + model[2, 1]
    segment <- rbind.data.frame(c(segment_x, segment_y))
    names(segment) <- c("x", "xmin", "y", "ymin")

    # Set error bar width and height
    # error_width <- sum(signal * c(-1, 1)) / 100
    # error_height <- sum(range(data$gamma_dose) * c(-1, 1)) / 100

    ggplot(
      data = data,
      mapping = aes(x = .data[[signal_value]], y = .data$gamma_dose,
                    label = .data$name)) +
      geom_segment(
        data = segment,
        mapping = aes(x = .data$x, xend = .data$xmin,
                      y = .data$y, yend = .data$ymin),
        colour = "red",
        inherit.aes = FALSE
      ) +
      geom_pointrange(
        mapping = aes(ymin = .data$gamma_dose - .data$gamma_error,
                      ymax = .data$gamma_dose + .data$gamma_error),
        colour = "red") +
      geom_errorbarh(
        mapping = aes(xmin = .data[[signal_value]] - .data[[signal_error]],
                      xmax = .data[[signal_value]] + .data[[signal_error]]),
        height = 0,
        colour = "red") +
      labs(x = sprintf("Signal [%s]", threshold), y = "Dose rate [\u03BCGy/y]")
  }
)
