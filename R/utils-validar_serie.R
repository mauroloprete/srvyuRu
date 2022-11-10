#' Validar serie
#' @name validar_serie.
#' @rdname validar_serie.
#' @keywords utils
#' @param .data A tibble.
#' @param .vars variable.
#' @return Devuelve un tibble.
#' @export

validar_serie. <- function(.svy,
                           .vars) {
  map_dfr.(
    .svy,
    .f = function(svy) {
      load_base.(
        svy
      ) %>%
        normalize_names.() %>%
        names() %>%
        assign(
          "svy_name",
          .,
          envir = parent.env(
            environment()
          )
        )

      tidytable(
        Variable = .vars,
        Validacion = .vars %in% svy_name,
        year = rep(
          svy,
          length(
            .vars
          )
        )
      )
    }
  )
}
