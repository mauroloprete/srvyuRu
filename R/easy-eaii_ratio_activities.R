#' Estimate the proportion of innovative companies by type of activity
#' @name estimate_ratio_activities.
#' @rdname estimate_ratio_activities.
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
#'   estimate_ratio_activities.(
#'     eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export


estimate_ratio_activities. <- function( # nolint
                                       .data,
                                       .eaii,
                                       .by = NULL,
                                       .recipe = "actividades",
                                       .vars = c(
                                         "IDinterna",
                                         "IDexterna",
                                         "bienescapital",
                                         "hardware",
                                         "software",
                                         "transferencia",
                                         "propiedadintelectual",
                                         "ingenieria",
                                         "gestion",
                                         "capacitacion",
                                         "marketing",
                                         "TICS",
                                         "id",
                                         "ad_software"
                                       )) {
  if (
    is.null(.vars)
  ) {
    print(
      "No variables to calculate"
    )
  } else {
    .data %<>%
      set_weight.(
        .eaii = {{ .eaii }}
      ) %>%
      load_recipes_eaii.(
        {{ .eaii }},
        .recipe = {{ .recipe }}
      ) %>%
      stats_proportion.(
        .vars,
        .filter_labels = "1"
      )
  }



  return(
    .data
  )
}
