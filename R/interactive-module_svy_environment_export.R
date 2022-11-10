
environment_exportServer <- function( # nolint
                                     id,
                                     current_id) {
  moduleServer(
    id,
    function(input,
             output,
             session) {

      # ! Reacción frente a current_id

      # En el objeto current_id es un objeto dentro del input que tiene
      # el id del botón y número de fila de la tabla de la vista previa.
      # En el caso que el objeto deje de ser nulo y además se haya
      # seleccionado uno de los botones del tipo export_
      # se desplegá una advertencia con que se ha borrado la
      # encuesta

      observeEvent(
        current_id(),
        {
          req(
            !is.null(current_id()) & stringr::str_detect(
              current_id(),
              pattern = "export_"
            )
          )

          f7Dialog(
            id = "dialog_export",
            title = "Encuesta exportada",
            type = "alert",
            text = "La encuesta ha sido exportada con éxito",
          )

          # Obtener el número de fila del environment
          # y luego exportarlo al global envir.

          id_row <- reactive(
            str_extract(
              current_id(),
              pattern = "[0-9]+"
            ) %>%
              as.numeric()
          )

          eapply(
            svy_env,
            FUN = list_svy
          ) %>%
            bind_rows.(
              .id = "Encuesta"
            ) %>%
            slice.(
              id_row()
            ) %>%
            pull.(
              Encuesta
            ) %>%
            rlang::parse_expr() %>%
            rlang::eval_tidy(
              env = svy_env
            ) %>%
            assign(
              paste(
                attributes(.)$survey,
                str_replace(
                  attributes(.)$edicion,
                  "-",
                  "_"
                ),
                sep = "_"
              ),
              .,
              envir = .GlobalEnv
            )
        }
      )
    }
  )
}
