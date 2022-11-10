#' Estimate the result of innovation
#' @name estimate_ratio_result_innovation.
#' @rdname estimate_ratio_result_innovation.
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
#'   estimate_ratio_result_innovation.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export





estimate_ratio_result_innovation. <- function( # nolint
                                              .data,
                                              .eaii,
                                              .by = NULL,
                                              .recipe = "resultado") { # nolint

  .data %<>%
    load_recipes_eaii.(
      {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    stats_proportion.(
      c(
        "innovaproducto",
        "innovaproceso"
      ),
      .filter_labels = "1"
    )

  return(
    .data
  )
}
