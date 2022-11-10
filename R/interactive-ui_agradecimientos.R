#' The application User-Interface_tabs
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import gt
#' @noRd
#'

agradecimientos <- function() {
  f7Tab(
    tabName = "Agradecimientos",
    icon = f7Icon("suit_heart"),
    active = TRUE,
    headerUI(
      "Agradecimientos"
    ),
    f7Card(
      htmlOutput("citation")
    )
  )
}
