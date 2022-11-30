#' Clasificación de CIIU Revisión 3 a CIIU Revisión 4 adaptada por el INE.
#'
#' @name get_ciiu4.
#' @rdname get_ciiu4.
#' @keywords utils
#' @param .data A tibble.
#' @param .ciiu3 Variable que tenga la clasificación ciiu3.
#' @param .ID número identificatorio del individuo de la muestra.
#' @examples
#' load_base.(
#'   "2007-2009-ser"
#' ) %>%
#'   to_ciiu4.(
#'     ClaseAct,
#'     "INE"
#'   )
#' @return Devuelve un tibble.
#'
#'
get_ciiu4. <- function(.data,
                       .ciiu3,
                       .ID) {


   str <- glue::glue(
    "
      .data %>%
        mutate.(
          'ciiu3_ine' = trunc(
            as.numeric(
              {.ciiu3}
            )
          )
        ) %>%
        left_join.(
          y = correspondencia
        ) %>%
        slice_head.(
          1,
          .by = {.ID}
        )
    ",
    .ciiu3 = .ciiu3,
    .ID = .ID
   )
  
    rlang::parse_expr(str) %>% rlang::eval_tidy()


}



#' Clasificación de CIIU Revisión 3 a CIIU Revisión 4 adaptada por el INE.
#'
#' @name to_ciiu4.
#' @rdname to_ciiu4.
#' @keywords utils
#' @param .data A tibble.
#' @param .ciiu3 Variable que tenga la clasificación ciiu3.
#' @param .ID número identificatorio del individuo de la muestra.
#' @examples
#' load_base.(
#'   "2007-2009-ser"
#' ) %>%
#'   to_ciiu4.(
#'     ClaseAct,
#'     "INE"
#'   )
#' @return Devuelve un tibble.
#' @export


to_ciiu4. <- function( # nolint
                      .data,
                      .ciiu3 = NULL,
                      .ID = NULL, # nolint
                      .eaii = NULL) {

  #     if (rlang::is_empty(.ciiu3) & rlang::is_empty(.ID)) {
  #     rlang::abort(
  #         crayon::red(
  #             "El argumento .ciiu3 ó .ID estan vacios,por favor indique ambos " # nolint
  #         )
  #     )
  # }

  if (
    is.null(.eaii)
  ) {
    .data %<>%
      get_ciiu4.(
        .ciiu3,
        .ID
      )

      return(.data)
  } else {
    if (.eaii == "1998-2000") {
      .data %<>%
        get_ciiu4.(
          .ciiu3 = rama1_re,
          .ID = NRO_INE
        )
    }

    if (.eaii == "2001-2003") {
      .data %<>%
        get_ciiu4.(
          .ciiu3 = rama,
          .ID = NRO_INE
        )
    }

    if (.eaii == "2004-2006") {
      .data %<>%
        get_ciiu4.(
          .ciiu3 = clase,
          .ID = nro_ine
        )
    }

    if (
      .eaii %in% c(
        "2007-2009",
        "2010-2012",
        "2013-2015",
        "2016-2018"
      )
    ) {
      .data
    }

    return(.data)
  }
}

