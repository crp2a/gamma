#' Launch the Shiny App
#'
#' A wrapper for shiny::shinyApp()
#' @examples \dontrun{launch_app()}
#' @return shiny application object
#' @author N. Frerebeau
#' @export
launch_app <- function() {
  shinyApp(ui = shiny_ui, server = shiny_server)
}
