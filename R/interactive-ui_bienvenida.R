#' The application User-Interface_tabs
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import gt
#' @noRd
#'
#'

bienvenida <- function() {

  # TODO : Mejorar mensaje de bienvenida

  f7Tab(
    tabName = "Bienvenida",
    icon = f7Icon("house"),
    active = TRUE,
    headerUI(
      "¡Bienvenido al procesamiento interactivo de srvyuRu!"
    ),
    br(),
    f7Card(
      title = h3("Sobre el uso del paquete"),
      f7Align(
        p(
          "El objetivo de este paquete es brindar una herramienta para procesar encuestas provenientes del INE.", # nolint
          br(),
          "Su desarrollo principal fue enfocado a la EAII (Encuesta de Actividades de Inversión e Innovación", # nolint
          br(),
          "También puede utilizarse para la ECH (Encuesta Continua de Hogares) y la ENDIS (Encuesta de Nutrición,Desarrollo Infantil y Salud)" # nolint
        ),
        side = "justify"
      ),
      footer = f7Link(
        "Para mas información, visitar aquí",
        "https://cognus.gitlab.io/proyectos/anii/srvyuru/"
      )
    ),
    br(),
    f7Card(
      title = h3("Sobre el uso del widget"),
      f7Align(
        p(
          "Con la ayuda de este este widget podrás realizar el mismo procesamiento que programando con las funciones de nuestro paquete.", # nolint
          "El mismo cuenta con una sección de carga, donde seleccionaras el archivo de la encuesta o lo obtendrás mediante una API.", # nolint
          br(),
          "Una vez cargada la encuesta se pueden crear diferentes variables, mediante el proceso interactivo.", # nolint
          "En él, pueden obtenerse variables cargando las recetas pre-cargadas ó creando las mismas manualmente,", # nolint
          "una vez hecho el preprocesamiento, puede realizar las estimaciones que desee con los estimadores poblacionales, ya sea conteos, totales, medias, proporciones o percentiles.", # nolint
          "Para futuros desarrollos se espera poder generar una 'comunidad' con diferentes recetas aportadas por los usuarios." # nolint
        ),
        side = "justify"
      )
    )
  )
}
