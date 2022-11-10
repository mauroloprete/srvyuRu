environment_diccUI <- function( # nolint
                               id) {
  f7Popup(
    id = NS(
      id,
      "popup_dicc"
    ),
    f7Card(
      title = h2("Diccionario"),
      footer = "Diccionario disponible aquí",
      gt_output(
        outputId = NS(
          id,
          "table_view"
        )
      )
    ),
    closeButton = FALSE
  )
}



environment_diccServer <- function( # nolint
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
      # seleccionado uno de los botones del tipo dicc_
      # se desplegá el popup para hara hacer las modificaciones
      # correspondientes.

      observeEvent(
        current_id(),
        {
          req(
            !is.null(current_id()) & stringr::str_detect(
              current_id(),
              pattern = "dicc_"
            )
          )

          id_row <- reactive(
            str_extract(
              current_id(),
              pattern = "[0-9]+"
            ) %>%
              as.numeric()
          )



          output$table_view <- render_gt(
            { # nolint


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
                lapply(
                  X = .,
                  FUN = list_metadata
                ) %>%
                bind_rows.(
                  .id = "Variable"
                ) %>%
                set_names(
                  c(
                    "Variable",
                    "Tipo",
                    "Datos no nulos"
                  )
                ) %>%
                mutate.(
                  Variable = tolower(
                    Variable
                  )
                ) %>%
                gt() %>%
                fmt_percent(
                  column = `Datos no nulos`,
                  decimals = 1
                ) %>%
                tab_style(
                  locations = cells_column_labels(
                    columns = everything()
                  ),
                  style = list(
                    cell_fill(color = "#40555D"),
                    cell_text(
                      weight = "bold",
                      color = "#FFFFFF"
                    )
                  )
                ) %>%
                tab_style(
                  locations = cells_body(
                    Variable
                  ),
                  style = list(
                    cell_text(
                      weight = "bold"
                    )
                  )
                ) %>%
                opt_table_font(
                  font = list(
                    google_font(
                      name = "Merriweather"
                    ),
                    "Cochin",
                    "Serif"
                  )
                )
            },
            width = "100%",
            height = "100%"
          )


          updateF7Popup(
            id = "popup_dicc"
          )
        }
      )
    }
  )
}
