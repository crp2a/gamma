#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  includeCSS("www/style.css"),
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
          shinyWidgets::pickerInput(
            inputId = "import_select",
            label = "Select",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            )
          ),
          checkboxInput("import_facet", "Display in a grid", value = FALSE),
          radioButtons("import_xaxis", "X axis", choices = c("chanel", "energy")),
          radioButtons("import_yaxis", "Y axis", choices = c("count", "rate"))
        ),
        mainPanel = mainPanel(
          downloadButton("import_export_plot", "Export plot"),
          downloadButton("import_export_table", "Export summary"),
          plotOutput("import_plot",
                     dblclick = "import_plot_dblclick",
                     brush = brushOpts(
                       id = "import_plot_brush",
                       resetOnNew = TRUE
                     )),
          tableOutput("import_summary")
        )
      )
    ),
    tabPanel(
      "2. Energy Calibration",
      icon = icon("bolt"),
      fluidPage(
        fluidRow(
          column(
            width = 4,
            wellPanel(
              h4("Set energy (keV)"),
              helpText(
                "You can adjust the energy scale by setting the energy values",
                "corresponding to at least 3 of the channels below",
                "and clicking on", dQuote("calibrate"), "."
              ),
              uiOutput("calib_input_peaks"),
              actionButton("calib_action", "Calibrate"),
              actionButton("calib_reset", "Restore")
            ),
          ),
          column(
            width = 8,
            column(
              width = 4,
              selectInput("calib_select", "Select a spectrum",
                          choices = NULL, selected = NULL,
                          multiple = FALSE)
            ),
            column(
              width = 8,
              style = "margin-top: 25px;",
              downloadButton("calib_export_table", "Export data"),
              downloadButton("calib_export_plot", "Export plot")
            ),
            column(
              width = 12,
              tabsetPanel(
                tabPanel(
                  "Raw spectrum",
                  plotOutput("calib_plot_peaks",
                             dblclick = "calib_plot_dblclick",
                             brush = brushOpts(
                               id = "calib_plot_brush",
                               resetOnNew = TRUE
                             ))
                ),
                tabPanel("Baseline corrected", plotOutput("calib_plot_baseline"))
              )
            )
          )
        ),
        fluidRow(
          h4("Peak detection parameters"),
          column(
            width = 3,
            h5("1. Drop chanels"),
            sliderInput("calib_slice_range", "Chanels to keep",
                        min = 1, max = 2048, value = c(1, 2048), step = 5
            ),
            h5("2. Transform signal"),
            selectInput("calib_stabilize_method", "Method", selected = 1,
                        choices = list(none = "none", `square root` = "sqrt"))
          ),
          column(
            width = 3,
            h5("3. Smooth signal"),
            selectInput("calib_smooth_method", "Method", selected = 1,
                        choices = list("savitzky", "unweighted", "weighted")),
            numericInput("calib_smooth_m", "Window size",
                         value = 21, min = 3, max = 50, step = 2),
            numericInput("calib_smooth_p", "Polynomial degree",
                         min = 1, max = 6, value = 2, step = 1)
          ),
          column(
            width = 3,
            h5("4. Remove baseline"),
            selectInput("calib_baseline_method", "Method", selected = 1,
                        choices = list("SNIP")),
            checkboxInput("calib_baseline_lls", "LLS", value = FALSE),
            checkboxInput("calib_baseline_decreasing", "Decreasing", value = FALSE),
            numericInput("calib_baseline_k", "Iterations",
                         value = 100, min = 10, max = 500, step = 10)
          ),
          column(
            width = 3,
            h5("5. Detect peaks"),
            selectInput("calib_peak_method", "Method", selected = 1,
                        choices = list("MAD")),
            numericInput("calib_peak_snr", "Signal-to-noise-ratio",
                         value = 2, min = 1, max = 5, step = 1),
            sliderInput("calib_peak_span", "Half window size",
                        min = 1, max = 100, value = 5, step = 1)
          )
        )
      )
    ),
    tabPanel(
      "3. Dose Rate Estimation",
      icon = icon("hourglass-half"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          selectInput("dose_curve", "Select a calibration curve", selected = 1,
                      choices = list(Choose = "",
                                     IRAMAT = c(BDX100 = "BDX100"),
                                     CEREGE = c(AIX100 = "AIX100"))),
          numericInput("dose_error", "Extra error term (%)",
                       min = 0, max = 100, value = 0, step = 1),
          tags$hr(),
          uiOutput("dose_info")
        ),
        mainPanel = mainPanel(
          column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = "dose_select",
              label = "Select spectra",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              )
            )
          ),
          column(
            width = 8,
            style = "margin-top: 25px;",
            downloadButton("dose_export", "Export results")
          ),
          column(
            width = 12,
            plotOutput("dose_plot_curve", hover = "dose_plot_hover"),
            htmlOutput("dose_table_dose")
          )
        ),
        position = "left",
        fluid = TRUE
      )
    ),
    tabPanel(
      "Settings",
      icon = icon("gear"),
      fluidPage(
        fluidRow(
          column(
            width = 3,
            h5("Print options"),
            numericInput("options_digits", "Significant digits",
                         value = 2, min = 1, max = 7, step = 1)
          ),
          column(
            width = 3,
            h5("Graphical output"),
            numericInput("options_fig_width", "Figure width", value = 7),
            numericInput("options_fig_height", "Figure height", value = 5),
            selectInput("options_fig_units", "Figure units",
                        choices = c("in", "cm", "mm"))
          ),
          column(
            width = 3
          ),
          column(
            width = 3
          ),
          column(
            width = 12,
            h5("Session information"),
            verbatimTextOutput("options_session")
          )
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
            img(src = "logo.png", width = "120px", alt = "gamma"),
            h4(paste("gamma", utils::packageVersion("gamma"), sep = " ")),
            tags$br(),
            tags$p("Source code:",
                   tags$a(href = "https://github.com/crp2a/gamma",
                          "https://github.com/crp2a/gamma")),
            tags$p(
              "This program is free software: you can redistribute it and/or
              modify it under the terms of the GNU General Public License as
              published by the Free Software Foundation, either version 3 of
              the License, or (at your option) any later version."
            ),
            tags$p(
              "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
              GNU General Public License for more details."
            ),
            tags$br(),
            tags$p(format(utils::citation("gamma"), bibtex = FALSE)[[1]]),
            tags$p(format(utils::citation("gamma"), bibtex = FALSE)[[2]]),
            tags$br(),
            tags$p(
              "This work received a state financial support
              managed by the Agence Nationale de la Recherche (France)
              throught the program Investissements d'avenir (ref. ",
              tags$a(href = "https://lascarbx.labex.u-bordeaux.fr/",
                     "ANR-10-LABX-52", .noWS = c("before", "after")), ")."
            )
          )
        )
      )
    )
  )
)
