environment_viewUI <- function( # nolint
                               id) {
  f7Popup(
    id = NS(
      id,
      "popup_view"
    ),
    title = h2("Vista previa"),
    f7Card(
      "Vista previa",
      gt_output(
        outputId = NS(
          id,
          "table_view"
        )
      )
    )
  )
}



environment_viewServer <- function( # nolint
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
      # seleccionado uno de los botones del tipo view_
      # se desplegá el visor de RStudio.

      observeEvent(
        current_id(),
        {
          req(
            !is.null(current_id()) & stringr::str_detect(
              current_id(),
              pattern = "view_"
            )
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
            rlang::parse_expr() %>%
            rlang::eval_tidy(
              env = svy_env
            ) %>%
            assign(
              "svy",
              .,
              envir = parent.env(
                environment()
              )
            )

          View(svy)
        }
      )
    }
  )
}
