#' The application User-Interface_tabs
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
#'

carga <- function() {
  f7Tab(
    tabName = "Preprocesamiento",
    icon = f7Icon("hammer"),
    active = TRUE,
    headerUI(
      "Preprocesamiento de datos"
    ),
    br(),
    svy_environmentUI(
      id = "svy_environment"
    ),
    f7Fabs(
      id = "bottom",
      position = "right-bottom",
      color = "#40555D",
      sideOpen = "top",
      extended = TRUE,
      label = "Cargar encuesta",
      f7Fab(
        "carga_eaii",
        "EAII",
        flag = "Encuesta de Actividades de Innovación e Inversión",
        width = "125%"
      ),
      f7Fab(
        "carga_ech", # TODO : Para cargar la encuesta descargar zip y pasar a RData # nolint
        "ECH",
        flag = "Encuesta Continua de Hogares",
        width = "125%"
      ),
      f7Fab(
        "carga_endis",
        "ENDIS",
        flag = "Encuesta de Nutrición, Desarrollo Infantil y Salud",
        width = "125%"
      )
    ),
    load.popupUI(
      id = "popup_eaii",
      title = "Encuesta de Actividades de Innovación e Inversión"
    ),
    load.popupUI(
      id = "popup_ech",
      title = "Encuesta Continua de Hogares"
    ),
    load.popupUI(
      id = "popup_endis",
      title = "Encuesta de Nutrición, Desarrollo Infantil y Salud"
    )
  )
}
