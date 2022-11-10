#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import gt
#' @noRd
#'
#'

app_ui <- function(request, session) {
  tagList(
    f7Page(
      title = "srvyuRu",
      f7TabLayout(
        navbar = f7Navbar(
          title = "srvyuRu",
          subtitle = "Procesamiento interactivo",
          transparent = TRUE,
          left_panel = TRUE,
          right_panel = FALSE
        ),
        f7Tabs(
          animated = TRUE,
          id = "wel",
          bienvenida(),
          carga(),
          resultados(),
          agradecimientos()
        )
      ),
      options = list(
        dark = FALSE,
        theme = "md",
        filled = TRUE,
        color = "#40555D"
      ),
      f7Login(
        id = "loginPage",
        title = "Bienvenido a srvyuRu",
        startOpen = FALSE
      ),
      preloader = TRUE,
      loading_duration = 0.1,
      get_id()
    )
  )
}
