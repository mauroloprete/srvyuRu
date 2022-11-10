
environment_deleteServer <- function(id,
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
      # seleccionado uno de los botones del tipo del_
      # se desplegá una advertencia con que se ha borrado la
      # encuesta

      observeEvent(
        current_id(),
        {
          req(
            !is.null(current_id()) & stringr::str_detect(
              current_id(),
              pattern = "del_"
            )
          )

          f7Dialog(
            id = "dialog_delete",
            title = "Encuesta eliminada",
            type = "alert",
            text = "La encuesta ha sido eliminada con éxito",
          )

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
            glue(
              "rm({svy},envir = svy_env)",
              svy = .
            ) %>%
            rlang::parse_expr() %>%
            rlang::eval_tidy()
        }
      )
    }
  )
}
