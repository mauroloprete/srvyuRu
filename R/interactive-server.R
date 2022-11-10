#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import gt
#' @import rlang
#' @import tidytable
#' @noRd


app_server <- function(input,
                       output,
                       session) {

  # ! Popups de carga de archivos ---------------------------------

  # Se fija esta opción para la carga de la ECH.

  options(
    shiny.maxRequestSize = 200 * 1024^2
  )

  #  Guardar encuesta en environment svy_env()

  new_environment() %>%
    assign(
      "svy_env",
      .,
      envir = .GlobalEnv
    )

  # * ---------------------------------------------------------

  # ! Carga de la EAII ----------------------------------------

  load.popupServer(
    id = "popup_eaii",
    svy_type = "eaii",
    button = reactive(input$carga_eaii)
  )

  observeEvent(
    input$carga_eaii,
    {
      # Se Cierra el botón
      updateF7Fabs(
        "bottom"
      )
    }
  )

  # * ---------------------------------------------------------

  # ! Carga de la ECH -----------------------------------------

  load.popupServer(
    id = "popup_ech",
    svy_type = "ech",
    button = reactive(input$carga_ech)
  )

  observeEvent(
    input$carga_ech,
    {
      # Se Cierra el botón
      updateF7Fabs(
        "bottom"
      )
    }
  )

  # * ---------------------------------------------------------

  # ! Carga de la ENDIS ---------------------------------------

  load.popupServer(
    id = "popup_endis",
    svy_type = "endis",
    button = reactive(input$carga_endis)
  )

  observeEvent(
    input$carga_endis,
    {
      # Se Cierra el botón
      updateF7Fabs(
        "bottom"
      )
    }
  )

  # * ---------------------------------------------------------


  # ! Ambiente de trabajo de las encuestas cargadas ---------------

  # * ---------------------------------------------------------

  # ! Tabla que presenta todas las encuestas

  # Dentro de este modulo se hacen los llamados de los acctionButtons

  svy_environmentServer(
    id = "svy_environment"
  )

  # * ---------------------------------------------------------



  # * -------------------------------------------------------------

  # ! Remover ambiente al parar la aplicación

  onStop(
    function() {
      message(
        crayon::red(
          "¡Gracias!"
        )
      )

      # ! Se elimina el ambiente de trabajo de las encuestas cargadas

      rm(
        svy_env,
        envir = .GlobalEnv
      )
    }
  )
}
