#' Shiny App Server Function
#'
#' @param input provided by Shiny.
#' @param output provided by Shiny.
#' @param session provided by Shiny.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  # Import =====================================================================
  myData <- reactiveValues(spectra = NULL, raw = NULL)
  mySpectrum <- reactiveValues(plot = NULL, name = NULL, summary = NULL)
  myPeaks <- reactiveValues(spectrum = NULL, peaks = NULL,
                            name = NULL, data = NULL, chanels = NULL,
                            plot_spectrum = NULL, plot_baseline = NULL)
  # Event ----------------------------------------------------------------------
  observeEvent(input$import_files, {
      file <- input$import_files
      # Return a GammaSpectra object
      spc_name <- tools::file_path_sans_ext(file$name)
      spc_data <- read(file$datapath)
      spc_data <- methods::as(spc_data, "GammaSpectra")
      set_name(spc_data) <- spc_name
      # Store data
      myData$spectra <- spc_data
      myData$raw <- spc_data
      # Update UI
      updateSelectInput(session, "import_select",
                        choices = spc_name, selected = spc_name)
      updateSelectInput(session, "calib_select",
                        choices = spc_name, selected = spc_name[[1]])
  })
  observeEvent(
    c(
      input$import_facet,
      input$import_select,
      input$import_xaxis,
      input$import_yaxis,
      input$calib_action,
      input$calib_reset
    ),
    {
      mySpectrum$summary <- summarise(myData$spectra[input$import_select])
      mySpectrum$name <- get_name(myData$spectra)
      mySpectrum$plot <- plot(myData$spectra,
                              xaxis = input$import_xaxis,
                              yaxis = input$import_yaxis,
                              select = input$import_select,
                              facet = input$import_facet) +
        ggplot2::theme_bw()
    },
    ignoreInit = TRUE
  )
  # Render ---------------------------------------------------------------------
  output$import_plot <- renderPlot(
    { mySpectrum$plot }
  )
  output$import_summary <- renderTable(
    { mySpectrum$summary },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE
  )
  output$import_export <- downloadHandler(
    filename = paste0(mySpectrum$name, ".png"),
    content = function(file) {
      ggsave(file, plot = mySpectrum$plot,
             width = 7, height = 5, units = "in")
    },
    contentType = "image/png"
  )
  # Energy calibration =========================================================
  # Event ----------------------------------------------------------------------
  observeEvent(
    c(
      input$calib_select,
      input$calib_action,
      input$calib_reset
    ),
    {
      # Get a GammaSpectrum object
      spc_raw <- myData$spectra[[input$calib_select]]
      spc_chanels <- get_chanels(spc_raw)
      # Drop chanels
      n <- -input$calib_slice_range
      index <- seq(from = n[[1]], to = n[[2]], by = -1)
      spc_sliced <- slice_signal(spc_raw, index)
      # Transform intensities
      trans <- switch(input$calib_stabilize_method,
                      none = function(x) x,
                      sqrt = sqrt)
      spc_transformed <- stabilize_signal(spc_sliced, transformation = trans)
      # Smooth intensities
      spc_smooted <- smooth_signal(
        spc_transformed,
        method = input$calib_smooth_method,
        m = input$calib_smooth_m,
        p = input$calib_smooth_p
      )
      # Remove baseline
      spc_baseline <- remove_baseline(
        spc_smooted,
        method = input$calib_baseline_method,
        LLS = input$calib_baseline_lls,
        decreasing = input$calib_baseline_decreasing,
        k = input$calib_baseline_k
      )
      # Detect peaks
      spc_peaks <- find_peaks(
        spc_baseline,
        method = input$calib_peak_method,
        SNR = input$calib_peak_snr,
        span = round(input$calib_peak_span * spc_chanels)
      )
      myPeaks$spectrum <- spc_raw
      myPeaks$peaks <- spc_peaks
      myPeaks$chanels <- spc_chanels
      myPeaks$name <- input$calib_select
      myPeaks$data <- methods::as(spc_raw, "data.frame")
      myPeaks$plot_spectrum <- plot(spc_sliced, spc_peaks) + theme_bw()
      myPeaks$plot_baseline <- plot(spc_baseline, spc_peaks) + theme_bw()
    },
    ignoreInit = TRUE
  )
  observeEvent(input$calib_select, {
    updateSliderInput(session, "calib_slice_range",
                      max = get_chanels(myData$spectra[[input$calib_select]]))
  }, ignoreInit = TRUE)
  observeEvent(input$calib_action, {
    if (!is.null(myPeaks$peaks)) {
      spc <- myPeaks$spectrum
      chanel <- myPeaks$peaks@chanel
      energy <- vapply(X = chanel, FUN = function(i) {
        input[[paste0("calib_peak_", i)]]
      }, FUN.VALUE = numeric(1))
      peaks <- methods::initialize(myPeaks$peaks, energy = energy)
      # Calibrate energy scale
      spc_calib <- calibrate_energy(spc, peaks)
      # Update spectrum
      myData$spectra[[input$calib_select]] <- spc_calib
    }
  })
  observeEvent(input$calib_reset, {
    myData$spectra[[input$calib_select]] <- myData$raw[[input$calib_select]]
  })
  # Render ---------------------------------------------------------------------
  output$calib_plot_peaks <- renderPlot(
    { myPeaks$plot_spectrum }
  )
  output$calib_plot_baseline <- renderPlot(
    { myPeaks$plot_baseline }
  )
  output$calib_input_peaks <- renderUI({
    if (!is.null(myPeaks$peaks)) {
      peaks <- methods::as(myPeaks$peaks, "data.frame")
      lapply(
        X = seq_len(nrow(peaks)),
        FUN = function(i, peaks) {
          chanel <- peaks$chanel[[i]]
          energy <- peaks$energy[[i]]
          numericInput(paste0("calib_peak_", chanel),
                       paste0("Chanel ", chanel), value = energy)
        },
        peaks
      )
    }
  })
  output$calib_export <- downloadHandler(
    filename = paste0(myPeaks$name, ".csv"),
    content = function(file) {
      utils::write.csv(myPeaks$data, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # Dose rate prediction =======================================================
  doseData <- reactive({
    if (!is.null(myData$spectra)) {
      predict_dose(curveData(), myData$spectra,
                   epsilon = input$dose_error / 100, simplify = TRUE)
    }
  })
  curveData <- reactive({
    tmp <- new.env()
    file <- system.file("data", paste0(input$dose_curve, ".rda"),
                        package = "gamma")
    load(file = file, envir = tmp)
    tmp[[ls(tmp)[1]]]
  })
  # Render ---------------------------------------------------------------------
  output$dose_plot_curve <- renderPlot({
    plot(curveData()) +
        ggplot2::theme_bw()
  })
  output$dose_table_curve_coef <- renderTable(
    {
      coef <- summary(curveData()[["model"]])$coefficients
      rownames(coef) <- c("intercept", "slope")
      coef
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = TRUE, colnames = TRUE, digits = 5
  )
  output$dose_table_curve_rsquared <- renderTable(
    {
      meta <- summary(curveData()[["model"]])
      cbind.data.frame(
        `residual standard error` = meta$sigma,
        `multiple R-squared` = meta$r.squared,
        `adjusted R-squared` = meta$adj.r.squared
      )
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE, digits = 5
  )
  output$dose_table_curve_data <- renderTable(
    { curveData()[["data"]] },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE, digits = 2
  )
  output$dose_table_dose <- renderTable(
    { doseData() },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE, digits = 2
  )
  output$dose_export <- downloadHandler(
    filename = "dose_rate.csv",
    content = function(file) {
      utils::write.csv(doseData(), file, fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # About ======================================================================
  output$about_logo <- renderImage(
    {
      list(
        src = system.file("/man/figures/", "logo.png", package = "gamma"),
        width = "120px",
        height = "136px",
        alt = "gamma"
      )
    },
    deleteFile = FALSE
  )
  output$about_version <- renderText({
    paste("gamma", utils::packageVersion("gamma"), sep = " ")
  })
  output$about_license <- renderUI({
    link_source <- "https://github.com/crp2a/gamma"
    list(
      tags$p("Source code:", tags$a(href = link_source, link_source)),
      tags$p(
        "This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version."
      ),
      tags$p(
        "This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
        GNU General Public License for more details."
      )
    )
  })
  output$about_citation <- renderUI({
   lapply(X = format(utils::citation("gamma"), bibtex = FALSE),
          FUN = tags$p)
  })
  output$about_lascarbx <- renderUI({
    tags$p(
      "This work received a state financial support
      managed by the Agence Nationale de la Recherche (France)
      throught the program Investissements d'avenir (ref. ",
      tags$a(href = "https://lascarbx.labex.u-bordeaux.fr/", "ANR-10-LABX-52"),
      ")."
    )
  })
}
