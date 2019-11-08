#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  navbarPage(
    "gamma",
    tabPanel(
      "1. Import",
      icon = icon("upload"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          fileInput('import_files', 'Import spectrum file(s)', multiple = TRUE,
                    accept = c('.cnf', '.CNF', '.tka', '.TKA')),
          tags$hr(),
          selectInput("import_select", "Select one or more spectra",
                      choices = NULL, selected = NULL, multiple = TRUE),
          checkboxInput("import_facet", "Display in a grid", value = FALSE),
          radioButtons("import_xaxis", "X axis", choices = c("chanel", "energy")),
          radioButtons("import_yaxis", "Y axis", choices = c("count", "rate"))
        ),
        mainPanel = mainPanel(
          downloadButton("import_export_plot", "Export plot"),
          downloadButton("import_export_table", "Export summary"),
          plotOutput("import_plot"),
          tableOutput("import_summary")
        )
      )
    ),
    tabPanel(
      "2. Energy Calibration",
      icon = icon("bolt"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h4("Set energy (keV)"),
          uiOutput("calib_input_peaks"),
          actionButton("calib_action", "Calibrate"),
          actionButton("calib_reset", "Restore")
        ),
        mainPanel = mainPanel(
          fluidRow(
            column(width = 4,
                   selectInput("calib_select", "Select a spectrum",
                               choices = NULL, selected = NULL,
                               multiple = FALSE)),
            column(width = 8,
                   style = "margin-top: 25px;",
                   downloadButton("calib_export_table", "Export data"),
                   downloadButton("calib_export_plot", "Export plot"))
          ),
          fluidRow(
            tabsetPanel(
              tabPanel("Raw spectrum", plotOutput("calib_plot_peaks")),
              tabPanel("Baseline corrected", plotOutput("calib_plot_baseline"))
            )
          )
        ),
        position = "left",
        fluid = TRUE
      ),
      fluidRow(
        column(
          width = 3,
          h4("1. Drop chanels"),
          sliderInput("calib_slice_range", "Chanels to drop",
                      min = 1, max = 2000, value = c(1, 35), step = 5
          ),
          h4("2. Transform signal"),
          selectInput("calib_stabilize_method", "Method", selected = 1,
                      choices = list(none = "none", `square root` = "sqrt"))
        ),
        column(
          width = 3,
          h4("3. Smooth signal"),
          selectInput("calib_smooth_method", "Method", selected = 1,
                      choices = list("savitzky", "unweighted", "weighted")),
          numericInput("calib_smooth_m", "Window size",
                       value = 5, min = 3, max = 10, step = 2),
          numericInput("calib_smooth_p", "Polynomial degree",
                       min = 1, max = 6, value = 2, step = 1)
        ),
        column(
          width = 3,
          h4("4. Remove baseline"),
          selectInput("calib_baseline_method", "Method", selected = 1,
                      choices = list("SNIP")),
          checkboxInput("calib_baseline_lls", "LLS", value = FALSE),
          checkboxInput("calib_baseline_decreasing", "Decreasing", value = FALSE),
          numericInput("calib_baseline_k", "Iterations",
                       value = 100, min = 10, max = 500, step = 10)
        ),
        column(
          width = 3,
          h4("5. Detect peaks"),
          selectInput("calib_peak_method", "Method", selected = 1,
                      choices = list("MAD")),
          numericInput("calib_peak_snr", "Signal-to-noise-ratio",
                       value = 2, min = 1, max = 5, step = 1),
          sliderInput("calib_peak_span", "Half window size",
                      min = 1, max = 100, value = 5, step = 1)
        )
      )
    ),
    tabPanel(
      "3. Dose Rate Estimation",
      icon = icon("hourglass-half"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          selectInput("dose_curve", "Select a calibration curve", selected = 1,
                      choices = list("BDX100", "AIX100")),
          numericInput("dose_error", "Extra error term (%)",
                       min = 0, max = 100, value = 0, step = 1),
          tags$hr(),
          selectInput("dose_select", "Select one or more spectra",
                      choices = NULL, selected = NULL, multiple = TRUE),
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel(
              "Dose rate",
              icon = icon("table"),
              tags$br(),
              downloadButton("dose_export", "Export results"),
              tableOutput("dose_table_dose")
            ),
            tabPanel(
              "Calibration curve",
              icon = icon("chart-line"),
              plotOutput("dose_plot_curve"),
              h4("Estimated coefficients"),
              tableOutput("dose_table_curve_coef"),
              h4("Summary statistics"),
              tableOutput("dose_table_curve_rsquared"),
              h4("Model data"),
              tableOutput("dose_table_curve_data")
            )
          )
        ),
        position = "left",
        fluid = TRUE
      )
    ),
    tabPanel(
      "Help",
      icon = icon("question-circle"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          uiOutput("help_topic")
        ),
        mainPanel = mainPanel(
          uiOutput("help_text")
        )
      )
    ),
    tabPanel(
      "About",
      icon = icon("info-circle"),
      fluidRow(
        column(
          width = 8,
          align = "center",
          offset = 2,
          wellPanel(
            # imageOutput("about_logo"),
            h4(textOutput("about_version")),
            tags$br(),
            uiOutput("about_license"),
            tags$br(),
            uiOutput("about_citation"),
            tags$br(),
            uiOutput("about_lascarbx")
          )
        )
      )
    )
  )
)
