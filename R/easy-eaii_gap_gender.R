#' Estimate the number of people engaged in I+D activities
#' @name estimate_gap_gender.
#' @rdname estimate_gap_gender.
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
#'   estimate_professionals_id.(
#'     .eaii = "2016-2018"
#'   )
#'
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_professionals_id.(
#'     .eaii = "2016-2018",
#'     .recipe = "profesionales_nivel"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export
#'
#'

estimate_gap_gender. <- function( # nolint
                                 .data,
                                 .eaii,
                                 .by = NULL,
                                 .recipe = "gap_gender") {
  .data %<>%
    load_recipes_eaii.(
      .eaii,
      {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = .eaii
    ) %>%
    stats_totals.(
      .vars = c(
        "total_profesionales_mujeres",
        "total_profesionales_hombres",
        "total_profid_mujeres",
        "total_profid_hombres",
        "total_innova_mujeres",
        "total_innova_hombres"
      ),
      .by = {{ .by }}
    )

  return(
    .data
  )
}
