#' The application User-Interface_tabs
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import gt
#' @noRd
#'


resultados <- function() {
  f7Tab(
    tabName = "Resultados",
    icon = f7Icon("graph_square"),
    active = TRUE,
    headerUI(
      "Resultados"
    )
  )
}
