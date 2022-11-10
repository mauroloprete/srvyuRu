#' Estimate labor market indicators based on the ECH
#' @name estimate_education_level.
#' @rdname estimate_education_level.
#' @keywords ech
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .ech Survey year
#' @param .by a variable to agroup the data
#' @param .recipe Recipe name
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .dir = here::here(
#'     "P_2019_Terceros.sav"
#'   )
#' ) %>%
#'   estimate_education_level.(
#'     .ech = "2019",
#'     .by = c("nomdpto", "e26")
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export


estimate_education_level. <- function( # nolint
                                      .data,
                                      .ech,
                                      .by = NULL,
                                      .recipe = "educacion") {
  if (
    {{ .ech }} %between% c(
      2006,
      2021
    )
  ) {
    .data %<>%
      load_recipes_ech.(
        .ech = {{ .ech }},
        .recipe = {{ .recipe }}
      ) %>%
      set_weight.(
        .ech = {{ .ech }}
      ) %>%
      mutate.(
        pob = "1"
      )


    map_dfr(
      .x = c(
        "bachillerato",
        "universidad"
      ),
      .f = function(x) {
        .data %>%
          stats_proportion.(
            .variable = x,
            .by = {{ .by }},
            .filter_labels = "1"
          )
      }
    )
  } else {
    message(
      glue::glue(
        crayon::red(
          "Variable no disponiible para el año {.ech}"
        )
      )
    )
  }
}
