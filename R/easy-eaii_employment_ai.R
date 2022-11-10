#' Estimate the number of people engaged in innovation activities
#' @name estimate_employment_ai.
#' @rdname estimate_employment_ai.
#' @keywords eaii
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .eaii Survey year
#' @param .recipe Recipe name
#' @param .zap_labels Boolean of labels to be removed
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_employment_ai.(
#'     .eaii = "2016-2018"
#'   )
#'
#' @return Returns a tibble with the created variables
#'
#' @export

estimate_employment_ai. <- function( # nolint
                                    .data,
                                    .eaii,
                                    .by = NULL,
                                    .recipe = "ocupados_ai") { # nolint

  .data %<>%
    load_recipes_eaii.(
      .eaii,
      {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = .eaii
    ) %>%
    stats_totals.(
      c(
        "ocupados_ai",
        "PTO_4.1.2018"
      ),
      .by = {{ .by }}
    )

  return(
    .data
  )
}
