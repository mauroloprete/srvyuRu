environment_modify_create_recipeUI <- function(id) {
  f7Tab(
    tabName = "Crear receta",
    f7Card(
      "Crear receta"
    )
  )
}

environment_modify_create_recipeServer <- function(id,
                                                   current_id) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      print("Hola")
    }
  )
}
