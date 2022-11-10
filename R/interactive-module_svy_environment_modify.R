environment_modifyUI <- function( # nolint
                                 id) {
  f7Popup(
    id = NS(
      id,
      "popup_modify"
    ),
    f7Card(
      title = h2("Modificar encuesta"),
      f7Tabs(
        environment_modify_create_recipeUI(
          NS(
            id,
            "modify_create_recipe"
          )
        ),
        environment_modify_load_recipeUI(
          NS(
            id,
            "modify_load_recipe"
          )
        ),
        f7Tab(
          tabName = "Modificar atributos",
          f7Card(
            "Modificar atributos"
          )
        )
      )
    ),
    closeButton = FALSE
  )
}



environment_modifyServer <- function( # nolint
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
      # seleccionado uno de los botones del tipo modify_
      # se desplegá el popup para hara hacer las modificaciones
      # correspondientes.

      observeEvent(
        current_id(),
        { # nolint
          req(
            !is.null(current_id()) & stringr::str_detect(
              current_id(),
              pattern = "modify_"
            )
          )

          updateF7Popup(
            id = "popup_modify"
          )


          environment_modify_load_recipeServer(
            id = "modify_load_recipe",
            current_id = current_id()
          )
        }
      )
    }
  )
}
