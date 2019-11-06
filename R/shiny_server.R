#' Shiny App Server Function
#'
#' @param input provided by Shiny.
#' @param output provided by Shiny.
#' @param session provided by Shiny.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  myData <- reactiveValues(spectra = NULL, raw = NULL)
  observeEvent(input$spc_files, {
      file <- input$spc_files
      # Return a GammaSpectra object
      spc_name <- vapply(X = file$name, FUN = tools::file_path_sans_ext,
                         FUN.VALUE = character(1))
      spc_data <- read(file$datapath)
      spc_data <- methods::as(spc_data, "GammaSpectra")
      set_reference(spc_data) <- spc_name
      myData$spectra <- spc_data
      myData$raw <- spc_data
  })
  peaksData <- reactive({
    k <- input$calib_select
    if (is.null(myData$spectra) || is.null(k) || nchar(k) == 0) {
      return(NULL)
    } else {
      # Get a GammaSpectrum object
      temp <- myData$spectra[[k]]
      # Drop chanels
      spc_chanels <- get_chanels(temp)
      n <- -input$calib_slice_range
      spc_sliced <- slice_signal(temp, seq(from = n[[1]], to = n[[2]], by = -1))
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
        span = round(input$calib_peak_span * get_chanels(temp))
      )
      return(list(spectrum = spc_sliced, baseline = spc_baseline,
                  peaks = spc_peaks, chanels = spc_chanels))
    }
  })
  doseData <- reactive({
    if (!is.null(myData$spectra)) {
      predict_dose(curveData(), myData$spectra, simplify = TRUE)
    }
  })
  curveData <- reactive({
    tmp <- new.env()
    file <- system.file("data", paste0(input$dose_curve, ".rda"),
                        package = "gamma")
    load(file = file, envir = tmp)
    tmp[[ls(tmp)[1]]]
  })
  # Widgets ====================================================================
  # Action ---------------------------------------------------------------------
  observeEvent(input$calib_action, {
    if (!is.null(peaksData())) {
      spc <- myData$spectra[[input$calib_select]]
      chanel <- peaksData()$peaks@chanel
      energy <- vapply(X = chanel, FUN = function(i) {
        input[[paste0("calib_peak_", i)]]
      }, FUN.VALUE = numeric(1))
      peaks <- methods::initialize(peaksData()$peaks, energy = energy)
      spc_calib <- calibrate_energy(spc, peaks)
      myData$spectra[[input$calib_select]] <- spc_calib
    }
  })
  observeEvent(input$calib_reset, {
    myData$spectra[[input$calib_select]] <- myData$raw[[input$calib_select]]
  })
  # Update ---------------------------------------------------------------------
  observe({
    if (!is.null(myData$spectra)) {
      updateSelectInput(session, "import_select",
                        choices = get_reference(myData$spectra))
      updateSelectInput(session, "calib_select",
                        choices = get_reference(myData$spectra))
    }
    if (!is.null(peaksData())) {
      updateSliderInput(session, "calib_slice_range",
                        max = peaksData()$chanels)
    }
  })
  # Output =====================================================================
  # Import ---------------------------------------------------------------------
  output$import_plot <- renderPlot({
    if (!is.null(myData$spectra)) {
      plot(myData$spectra, xaxis = input$import_xaxis, yaxis = input$import_yaxis,
           select = input$import_select, facet = input$import_facet) +
        ggplot2::theme_bw()
    }
  })
  output$import_summary <- renderTable(
    {
      if (!is.null(myData$spectra)) {
        summarise(myData$spectra[input$import_select])
      }
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE
  )
  # Energy calibration ---------------------------------------------------------
  output$calib_plot_peaks <- renderPlot({
    if (!is.null(peaksData())) {
      plot(peaksData()$spectrum, peaksData()$peaks) +
        ggplot2::theme_bw()
    }
  })
  output$calib_plot_baseline <- renderPlot({
    if (!is.null(peaksData())) {
      plot(peaksData()$baseline, peaksData()$peaks) +
        ggplot2::theme_bw()
    }
  })
  output$calib_input_peaks <- renderUI({
    if (!is.null(peaksData())) {
      peaks <- methods::as(peaksData()$peaks, "data.frame")
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
  # Dose rate prediction -------------------------------------------------------
  output$dose_plot_curve <- renderPlot({
    plot(curveData()) +
        ggplot2::theme_bw()
  })
  output$dose_table_curve <- renderTable(
    {
      curveData()[["data"]]
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE, digits = 2
  )
  output$dose_table_dose <- renderTable(
    {
      doseData()
    },
    spacing = "s", width = "auto",
    striped = TRUE, hover = TRUE, bordered = FALSE,
    rownames = FALSE, colnames = TRUE, digits = 2
  )
}
