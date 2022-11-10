#' Crear un grupo para luego particionar la poblaci'on y
#' calcular el total poblacional en cada momento
#' @name recode
#' @rdname recode
#' @keywords utils
#' @param .data A tibble.
#' @param .name Nombre de la variable que vamos a crear.
#' @param ... Condiciones para armar dicha variable
#' @param .keep
#' @examples
#' load_base.(
#'   "2016-2018"
#' ) %>%
#'   recode(
#'     var1,
#'     IB_3.1.1 == "pri" ~ "Privada",
#'     TRUE ~ "Publica/Mixta"
#'   ) %>%
#'   recode(
#'     var2,
#'     RAI_E.1_1 == 1 ~ "Si",
#'     RAI_E.1_2 == 1 ~ "Si",
#'     TRUE ~ "No"
#'   ) %>%
#'   tidytable::select.(
#'     var1,
#'     var2
#'   )
#' @return Devuelve un tibble.
#' @export


recode <- function(.data = NULL, .name = NULL, ..., .keep = "all", .before = NULL, .after = NULL) {

  # if (rlang::is_empty(.name) & rlang::is_empty(.ncond)) {
  #     rlang::abort(
  #         crayon::red(
  #             "El argumento .name ó .ncond estan vacios,por favor indique ambos " # nolint
  #         )
  #     )
  # }

  .data %>%
    mutate.(
      "{{.name}}" := tidytable::case_when.( # nolint
        !!!rlang::list2(
          ...
        )
      ),
      .keep = {{ .keep }}
    )
}
