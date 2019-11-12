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
  # shinyApp(ui = shiny_ui, server = shiny_server)
  shinyAppDir(
    system.file("shinyApp", package = "gamma"),
    options = list(width = 800, height = 600)
  )
}
