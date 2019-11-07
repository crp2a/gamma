# SHINY
#' @include AllClasses.R
NULL

#' Launch the Shiny App
#'
#' A wrapper for \code{\link[shiny]{shinyApp}}.
#' @examples \dontrun{launch_app()}
#' @return A \pkg{shiny} application object.
#' @family IO
#' @author N. Frerebeau
#' @export
launch_app <- function() {
  shinyApp(ui = shiny_ui, server = shiny_server)
}
