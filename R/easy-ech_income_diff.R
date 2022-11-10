#' Estimate labor market indicators based on the ECH
#' @name estimate_income_diff.
#' @rdname estimate_income_diff.
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


estimate_income_diff. <- function( # nolint
                                  .data,
                                  .ech,
                                  .by = NULL,
                                  .recipe = c(
                                    "educacion",
                                    "income"
                                  )) {
  if (
    {{ .ech }} %between% c(
      2011,
      2021
    )
  ) {
    .data %<>%
      load_recipes_ech.(
        .ech = {{ .ech }},
        .recipe = .recipe[1]
      ) %>%
      load_recipes_ech.(
        .ech = {{ .ech }},
        .recipe = .recipe[2]
      ) %>%
      set_weight.(
        .ech = {{ .ech }}
      )


    .data %>%
      filter.(
        f85 > 0
      ) %>%
      stats_mean.(
        .vars = c(
          "ingreso_total",
          "ingreso_hora"
        ),
        .by = c(
          "e26",
          "max_educativo"
        )
      ) %>%
      mutate.(
        e26 = ifelse.(
          e26 == 1,
          "Masculino",
          "Femenino"
        ),
        anio = .ech
      ) %>%
      pivot_wider.(
        names_from = e26,
        values_from = estimacion
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
