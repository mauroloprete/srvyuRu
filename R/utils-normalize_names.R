#' Normalize names
#' @name normalize_names.
#' @rdname normalize_names.
#' @keywords utils
#' @param .data A tibble.
#' @param .meth Nombre de la variable que vamos a crear.
#' @return Devuelve un tibble.
#' @export

normalize_names. <- function(.data,
                             meth = tolower) {
  message(
    crayon::blue(
      glue(
        "
                Normalizando nombres de variables
                \n
                use el nombre de variables en minuscula, ej : e26 y no E26
                "
      )
    )
  )

  .data %>%
    rlang::set_names(
      {{ meth }}
    )
}
