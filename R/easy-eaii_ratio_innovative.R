#' Estimate the percentage of innovative companies
#' @name estimate_ratio_innovative.
#' @rdname estimate_ratio_innovative.
#' @keywords eaii
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .eaii Survey year
#' @param .by a variable to agroup the data
#' @param .recipe Recipe name
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_ratio_innovative.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export




estimate_ratio_innovative. <- function( # nolint
                                       .data,
                                       .eaii,
                                       .by = NULL,
                                       .recipe = "realiza_innovacion") { # nolint

  .data %<>%
    load_recipes_eaii.(
      {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    stats_proportion.(
      "realiza_innovacion",
      .by = {{ .by }},
      .filter_labels = "1"
    )

  return(
    .data
  )
}


#' Estimate the percentage of innovative companies
#' @name estimate_proportion_innovative.
#' @rdname estimate_proportion_innovative.
#' @keywords eaii
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .eaii Survey year
#' @param .by a variable to agroup the data
#' @param .recipe Recipe name
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_ratio_innovative.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export

estimate_proportion_innovative. <- function( # nolint
                                            .data,
                                            .eaii,
                                            .by = NULL,
                                            .recipe = "innovative_bussiness") { # nolint

  .data %<>%
    load_recipes_eaii.(
      {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    stats_proportion.(
      "innovativa",
      .by = {{ .by }},
      .filter_labels = "1"
    )

  return(
    .data
  )
}
