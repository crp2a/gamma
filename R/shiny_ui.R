#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  navbarPage(
    "gamma",
    tabPanel(
      "Import",
      icon = icon("upload"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          fileInput('spc_files', 'Choose spectrum file(s)', multiple = TRUE,
                    accept = c('.cnf', '.CNF', '.tka', '.TKA')),
          tags$hr(),
          checkboxInput("import_facet", "Display in a grid", value = FALSE),
          selectInput("import_select", "Select a spectrum", choices = NULL,
                      multiple = TRUE),
          radioButtons("import_xaxis", "X axis", choices = c("chanel", "energy")),
          radioButtons("import_yaxis", "Y axis", choices = c("count", "rate"))
        ),
        mainPanel = mainPanel(
          plotOutput("import_plot"),
          tableOutput("import_summary")
        )
      )
    ),
    tabPanel(
      "Energy Calibration",
      icon = icon("bolt"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h4("Set energy"),
          uiOutput("calib_input_peaks"),
          actionButton("calib_action", "Calibrate"),
          actionButton("calib_reset", "Restore")
        ),
        mainPanel = mainPanel(
          fluidRow(
            selectInput("calib_select", "Select a spectrum",
                        choices = NULL, multiple = FALSE)
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
                      min = 1, max = 1024, value = c(1, 35), step = 5
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
                       value = 3, min = 3, max = 10, step = 2),
          numericInput("calib_smooth_p", "Polynomial degree",
                       min = 1, max = 6, value = 1, step = 1)
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
                      min = 0.01, max = 1, value = 0.05, step = 0.01)
        )
      )
    ),
    tabPanel(
      "Dose Rate",
      icon = icon("hourglass-half"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          selectInput("dose_curve", "Curve", selected = 1,
                      choices = list("BDX1", "AIX1"))
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel(
              "Dose rate",
              tableOutput("dose_table_dose")
            ),
            tabPanel(
              "View curve",
              plotOutput("dose_plot_curve"),
              tableOutput("dose_table_curve")
            )
          )
        ),
        position = "left",
        fluid = TRUE
      )
    ),
    tabPanel(
      "About",
      icon = icon("info-circle"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          "sidebar panel"
        ),
        mainPanel = mainPanel(
          "main panel"
        )
      )
    )
  )
)
