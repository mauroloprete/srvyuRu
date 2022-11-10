#' Estimate the result of Max innovation.
#' @name estimate_ratio_max_innovation.
#' @rdname estimate_ratio_max_innovation.
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
#'   estimate_ratio_max_innovation.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export





estimate_ratio_max_innovation. <- function( # nolint
                                           .data,
                                           .eaii,
                                           .by = NULL,
                                           .recipe = "max_innovacion") { # nolint

  .data %<>%
    load_recipes_eaii.(
      {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    stats_proportion.(
      .variable = c(
        "alcance_empresa",
        "alcance_mercado_local",
        "alcance_mercado_internacional"
      ),
      .filter_labels = "1"
    )

  return(
    .data
  )
}
