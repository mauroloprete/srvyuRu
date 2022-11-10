#' Estimate the proportion of public support
#' @name estimate_ratio_public_support.
#' @rdname estimate_ratio_public_support.
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
#'   estimate_ratio_public_support.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export


estimate_ratio_public_support. <- function( # nolint
                                           .data,
                                           .eaii,
                                           .by = NULL,
                                           .recipe = "apoyopublico") { # nolint

  .data %<>%
    load_recipes_eaii.(
      {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    filter.(
      !is.na(
        apoyopublico
      )
    ) %>%
    stats_proportion.(
      "apoyopublico",
      .filter_labels = "1"
    )



  return(
    .data
  )
}
