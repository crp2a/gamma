# SHINY
#' @include AllClasses.R
NULL

#' Launch the Shiny App
#'
#' A wrapper for \code{\link[shiny]{shinyAppDir}}.
#' @examples \dontrun{launch_app()}
#' @return A \pkg{shiny} application object.
#' @family shiny
#' @author N. Frerebeau
#' @export
launch_app <- function() {
  if (!require("shiny", quietly = TRUE)) {
    message("Shiny must be installed to run the app.")
    answer <- readline("Do you want to install Shiny? (Y/N) ")
    answer <- ifelse(answer %in% c("y", "Y", "1"), TRUE, FALSE)
    if (answer) {
      utils::install.packages("shiny")
    } else {
      stop("Shiny is needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }
  shiny::shinyAppDir(
    system.file("shinyApp", package = "gamma"),
    options = list(width = 800, height = 600)
  )
}
