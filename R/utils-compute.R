#' Compute a new variable
#' @name compute
#' @rdname compute
#' @keywords utils
#' @param .data A tibble.
#' @param .name Nombre de la variable que vamos a crear.
#' @param .keep
#' @return Devuelve un tibble.
#' @export

compute <- function(.data = NULL, .name = NULL, .var, .keep = "all") {

  # if (rlang::is_empty(.name) & rlang::is_empty(.ncond)) {
  #     rlang::abort(
  #         crayon::red(
  #             "El argumento .name ó .ncond estan vacios,por favor indique ambos " # nolint
  #         )
  #     )
  # }
  # args <- list2(...)

  .data %>%
    mutate.(
      "{{.name}}" := {{ .var }}, # nolint
      .keep = {{ .keep }}
    )
}
