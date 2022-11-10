#' Estimate labor market indicators based on the ECH
#' @name estimate_labor_indicators.
#' @rdname estimate_labor_indicators.
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
#'   estimate_labor_indicators.(
#'     .ech = "2019",
#'     .by = c("nomdpto", "e26")
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export


estimate_labor_indicators. <- function( # nolint
                                       .data,
                                       .ech,
                                       .by = NULL,
                                       .recipe = "labor",
                                       .load_recipe = TRUE) {
  if (.load_recipe) {
    .data %<>%
      load_recipes_ech.(
        .ech = {{ .ech }},
        .recipe = "labor"
      ) %>%
      set_weight.(
        .ech = {{ .ech }}
      )
  }

  pmap_dfr(
    list(
      x = c(
        "pea",
        "pd",
        "po"
      ),
      y = c(
        "pet",
        "pea",
        "pet"
      ),
      z = c(
        "TA",
        "TD",
        "TE"
      )
    ),
    .f = function(x, y, z) {
      .data %>%
        stats_ratio.(
          .num = !!rlang::sym(x),
          .den = !!rlang::sym(y),
          .name = !!rlang::sym(z),
          .by = {{ .by }},
          .pivot = FALSE
        )
    }
  ) %>%
    summarise.(
      across.(
        .cols = -{{ .by }},
        .fns = ~ sum(
          .x,
          na.rm = TRUE
        )
      ),
      .by = {{ .by }}
    )
}
