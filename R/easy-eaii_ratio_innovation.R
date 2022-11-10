#' Estimate the proportion of the type of innovation
#' @name estimate_ratio_innovation.
#' @rdname estimate_ratio_innovation.
#' @keywords eaii
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .eaii Survey year
#' @param .vars Variables to calculate
#' @param .by a variable to agroup the data
#' @param .recipe Recipe name
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_ratio_innovation.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#' @export


estimate_ratio_innovation. <- function( # nolint
                                       .data,
                                       .eaii,
                                       .vars = c(
                                         "producto",
                                         "proceso",
                                         "organizacion",
                                         "comercializacion"
                                       ),
                                       .by = NULL,
                                       .recipe = "tipo_innovacion") { # nolint

  if (
    is.null(.vars)
  ) {
    print(
      "No variables to calculate"
    )
  } else {
    .data %<>%
      load_recipes_eaii.(
        {{ .eaii }},
        .recipe = {{ .recipe }}
      ) %>%
      set_weight.(
        .eaii = {{ .eaii }}
      ) %>%
      stats_proportion.(
        .vars,
        .filter_labels = "1",
        .by = {{ .by }}
      )
  }


  return(
    .data
  )
}
