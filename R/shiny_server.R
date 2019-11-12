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
  myData <- reactiveValues(spectra = NULL, names = NULL, raw = NULL)
  myRangesCalib <- reactiveValues(x = NULL, y = NULL, expand = TRUE)
  myRangesImport <- reactiveValues(x = NULL, y = NULL, expand = TRUE)
  tmp <- tempfile()
  onSessionEnded(function() { unlink(tmp) })
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
      myData$names <- spc_name
      myData$raw <- spc_data
      # Update UI
      # updateSelectInput(session, "import_select",
      #                   choices = spc_name, selected = spc_name)
      shinyWidgets::updatePickerInput(session, "import_select",
                  choices = spc_name, selected = spc_name)
      updateSelectInput(session, "calib_select",
                        choices = spc_name, selected = spc_name[[1]])
      # updateSelectInput(session, "dose_select",
      #                   choices = spc_name, selected = spc_name)
      shinyWidgets::updatePickerInput(session, "dose_select",
                                      choices = spc_name, selected = spc_name)
  })
  observeEvent(input$import_plot_dblclick, {
    brush <- input$import_plot_brush
    if (!is.null(brush)) {
      myRangesImport$x <- c(brush$xmin, brush$xmax)
      myRangesImport$y <- c(brush$ymin, brush$ymax)
      myRangesImport$expand <- FALSE
    } else {
      myRangesImport$x <- NULL
      myRangesImport$y <- NULL
      myRangesImport$expand <- TRUE
    }
  })
  # Reactive -------------------------------------------------------------------
  mySpectrum <- reactive(
    {
      # req(myData$spectra)
      validate(
        need(!is.null(myData$spectra), "Please import one or more spectra."),
        need(!is.null(input$import_select) &&
               input$import_select != "" &&
               input$import_select %in% myData$names,
             "Please select at least one spectrum.")
      )
      list(
        summary = summarise(myData$spectra[input$import_select]),
        name = get_name(myData$spectra[input$import_select]),
        plot = plot(myData$spectra,
                    xaxis = input$import_xaxis,
                    yaxis = input$import_yaxis,
                    select = input$import_select,
                    facet = input$import_facet) +
          ggplot2::theme_bw()
      )
    }
  )
  # Render ---------------------------------------------------------------------
  output$import_plot <- renderPlot(
    { mySpectrum()$plot +
        ggplot2::coord_cartesian(xlim = myRangesImport$x, ylim = myRangesImport$y,
                                 expand = myRangesImport$expand) }
  )
  output$import_summary <- renderTable(
    { mySpectrum()$summary },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE
  )
  output$import_export_plot <- downloadHandler(
    filename = function() {
      ifelse(
        length(mySpectrum()$name) == 1,
        paste0(mySpectrum()$name, ".png"),
        "spectra.png"
      )
    },
    content = function(file) {
      ggsave(file, plot = mySpectrum()$plot,
             width = 7, height = 5, units = "in")
    },
    contentType = "image/png"
  )
  output$import_export_table <- downloadHandler(
    filename = "summary.csv",
    content = function(file) {
      utils::write.csv(mySpectrum()$summary, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # Energy calibration =========================================================
  # Reactive -------------------------------------------------------------------
  myPeaks <- reactive(
    {
      # Validation
      req(myData$spectra, input$calib_select)
      validate(
        need(!is.null(input$calib_smooth_m) && input$calib_smooth_m != "",
             "The window size must be set (smoothing)."),
        need(input$calib_smooth_m %% 2 != 0,
             "The window size must be an odd integer (smoothing)."),
        need(!is.null(input$calib_smooth_p) && input$calib_smooth_p != "",
             "The polynomial degree must be set (smoothing)."),
        need(!is.null(input$calib_baseline_k) && input$calib_baseline_k != "",
             "The number of iteration must be set (baseline)."),
        need(!is.null(input$calib_peak_snr) && input$calib_peak_snr != "",
             "The signal-to-noise-ratio must be set (peak searching).")
      )
      # Get a GammaSpectrum object
      spc_raw <- myData$spectra[[input$calib_select]]
      spc_chanels <- get_chanels(spc_raw)
      # Drop chanels
      n <- input$calib_slice_range
      index <- seq(from = n[[1]], to = n[[2]], by = 1)
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
        span = input$calib_peak_span * spc_chanels / 100
      )
      list(
        spectrum = spc_raw,
        peaks = spc_peaks,
        chanels = spc_chanels,
        name = input$calib_select,
        data = methods::as(spc_raw, "data.frame"),
        plot_spectrum = plot(spc_sliced, spc_peaks) + theme_bw(),
        plot_baseline = plot(spc_baseline, spc_peaks) + theme_bw()
      )
    }
  )
  # Event ----------------------------------------------------------------------
  observeEvent(input$calib_select, {
    req(myData$spectra, input$calib_select)
    max_chanel <- get_chanels(myData$spectra[[input$calib_select]])
    updateSliderInput(session, "calib_slice_range",
                      max = max_chanel, value = c(35, max_chanel))
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$calib_plot_dblclick, {
    brush <- input$calib_plot_brush
    if (!is.null(brush)) {
      myRangesCalib$x <- c(brush$xmin, brush$xmax)
      myRangesCalib$y <- c(brush$ymin, brush$ymax)
      myRangesCalib$expand <- FALSE
    } else {
      myRangesCalib$x <- NULL
      myRangesCalib$y <- NULL
      myRangesCalib$expand <- TRUE
    }
  })
  observeEvent(input$calib_action, {
    req(myPeaks())
    spc <- myPeaks()$spectrum
    chanel <- myPeaks()$peaks@chanel
    energy <- vapply(X = chanel, FUN = function(i) {
      input[[paste0("calib_peak_", i)]]
    }, FUN.VALUE = numeric(1))
    peaks <- methods::initialize(myPeaks()$peaks, energy = energy)
    # Calibrate energy scale
    spc_calib <- try(calibrate_energy(spc, peaks))
    # Update spectrum
    if (class(spc_calib) == "try-error") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error",
        text = spc_calib,
        type = "error"
      )
    } else {
      myData$spectra[[input$calib_select]] <- spc_calib
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Success",
        text = "The enegy scale has been adjusted.",
        type = "success"
      )
    }
  })
  observeEvent(input$calib_reset, {
    myData$spectra[[input$calib_select]] <- myData$raw[[input$calib_select]]
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Info",
      text = "The energy scale has been restored to its original values.",
      type = "info"
    )
  })
  # Render ---------------------------------------------------------------------
  output$calib_plot_peaks <- renderPlot(
    { myPeaks()$plot_spectrum +
        ggplot2::coord_cartesian(xlim = myRangesCalib$x, ylim = myRangesCalib$y,
                                 expand = myRangesCalib$expand) }
  )
  output$calib_plot_baseline <- renderPlot(
    { myPeaks()$plot_baseline }
  )
  output$calib_energy_message <- renderUI({
    req(myPeaks())
    msg <- paste("The spectrum %s %s an energy scale.")
    tags$div(
      if (is_calibrated(myPeaks()$spectrum)) {
        tags$p(sprintf(msg, input$calib_select, "has"),  class = "gamma-info")
      } else {
        tags$p(sprintf(msg, input$calib_select, "does not have"),
               class = "gamma-warning")
      },
      class = "gamma-message"
    )
  })
  output$calib_input_peaks <- renderUI({
    req(myPeaks())
    peaks <- methods::as(myPeaks()$peaks, "data.frame")
    lapply(
      X = seq_len(nrow(peaks)),
      FUN = function(i, peaks) {
        chanel <- peaks$chanel[[i]]
        # energy <- peaks$energy[[i]]
        numericInput(inputId = paste0("calib_peak_", chanel),
                     label = paste0("Chanel ", chanel), value = NA_real_)
      },
      peaks
    )
  })
  output$calib_export_table <- downloadHandler(
    filename = function() paste0(myPeaks()$name, ".csv"),
    content = function(file) {
      utils::write.csv(myPeaks()$data, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  output$calib_export_plot <- downloadHandler(
    filename = function() paste0(myPeaks()$name, ".png"),
    content = function(file) {
      ggsave(file, plot = myPeaks()$plot_spectrum,
             width = 7, height = 5, units = "in")
    },
    contentType = "image/png"
  )
  # Dose rate prediction =======================================================
  doseData <- reactive({
    req(myData$spectra, input$dose_error)
    predict_dose(curveData(), myData$spectra,
                 epsilon = input$dose_error / 100, simplify = TRUE)
  })
  curveData <- reactive({
    req(input$dose_curve)
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
    rownames = TRUE, colnames = TRUE
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
    rownames = FALSE, colnames = TRUE
  )
  output$dose_table_curve_data <- renderTable(
    { curveData()[["data"]] },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE
  )
  output$dose_table_dose <- renderTable(
    {
      req(input$dose_select)
      doseData()[input$dose_select, ]
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE
  )
  output$dose_export <- downloadHandler(
    filename = "dose_rate.csv",
    content = function(file) {
      req(input$dose_select)
      utils::write.csv(doseData()[input$dose_select, ], file,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # Help =======================================================================
  # https://stackoverflow.com/questions/49210495/show-documentation-pages-in-shiny-app
  myRd <- reactive({
    tools::Rd_db("gamma")
  })
  output$help_topic <- renderUI({
    selectInput("help_topic", "Select topic",
                choices = sub(".Rd", "", names(myRd())))
  })
  output$help_text <- renderUI({
    rd_file <- paste0(input$help_topic, ".Rd")
    req(rd_file %in% names(myRd()))
    tools::Rd2HTML(myRd()[[rd_file]], out = tmp, package = "gamma",
                   no_links = TRUE)
    includeHTML(tmp)
  })
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
