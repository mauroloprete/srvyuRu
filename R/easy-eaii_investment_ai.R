#' Estimate investment in innovation activities for the period
#' @name estimate_investment_ai.
#' @rdname estimate_investment_ai.
#' @keywords eaii
#' @keywords easy
#' @param .data Tibble with the survey
#' @param .eaii Survey year
#' @param .by Variable to group by
#' @param .recipe Recipe name
#' @param type Boolean of labels to be removed
#' @examples
#'
#'
#' devtools::load_all()
#' load_base.(
#'   .eaii = "2016-2018"
#' ) %>%
#'   estimate_investment_ai.(
#'     .eaii = "2016-2018",
#'     .by = "sector",
#'     type = "anii"
#'   )
#'
#' @return Returns a tibble with the created variables
#'
#' @export


estimate_investment_ai. <- function( # nolint
                                    .data,
                                    .eaii,
                                    .by = NULL,
                                    .recipe = "inversion",
                                    type = "anii",
                                    summarise = FALSE) { # nolint


  .data %<>%
    load_recipes_eaii.(
      .eaii = {{ .eaii }},
      .recipe = {{ .recipe }}
    ) %>%
    set_weight.(
      .eaii = {{ .eaii }}
    )

  str_split(
    {{ .eaii }},
    pattern = "-"
  )[[1]][1]:str_split(
    {{ .eaii }},
    pattern = "-"
  )[[1]][2] %>%
    as.numeric() %>%
    assign(
      "years",
      .,
      envir = parent.env(
        environment()
      )
    )

  if (max(years) <= 2009) {
    years %<>%
      max()
  }

  if ({{ .recipe }} == "inversion") {
    years %>%
      map_dfr(
        .f = function(x) {
          .data %>%
            to_usd.(
              !!rlang::sym(
                paste0(
                  "AI_",
                  x
                )
              ),
              .year = !!x,
              type = {{ type }}
            ) %>%
            stats_totals.(
              !!rlang::sym(
                paste0(
                  "AI_",
                  x,
                  "_usd"
                )
              ),
              .by = {{ .by }}
            )
        }
      ) %>%
      assign(
        "result",
        .,
        envir = parent.env(environment())
      )
  }


  if (summarise && {{ .recipe }} == "inversion") {
    result %<>%
      summarise.(
        estimacion = sum(estimacion)
      ) %>%
      mutate.(
        variable = "innova_usd_total"
      )
  }


  # TODO : Cargar receta para que sea de software, bienescapital, etc ...


  if ({{ .recipe }} == "inversion_act") {
    years %>%
      map_dfr(
        .f = function(x) {
          map_dfr(
            .x = c(
              "B.1_1",
              "B.1_2",
              "B.1_3",
              "B.1_4",
              "B.1_5",
              "B.1_6",
              "B.1_7",
              "B.1_8",
              "B.1_9"
            ),
            .f = function(j) {
              .data %>%
                mutate.(
                  across.(
                    .cols = where(is.numeric),
                    .fns = ~ replace_na.(.x, 0)
                  )
                ) %>%
                to_usd.(
                  !!rlang::sym(
                    paste0(
                      "AI_",
                      j,
                      "_",
                      x
                    )
                  ),
                  .year = !!x
                ) %>%
                stats_totals.(
                  !!rlang::sym(
                    paste0(
                      "AI_",
                      j,
                      "_",
                      x,
                      "_usd"
                    )
                  ),
                  .by = {{ .by }}
                )
            }
          )
        }
      ) %>%
      mutate.( # TODO : Mejorar esto con regex, esto es una cagada!!
        variable = str_sub(
          Variable,
          start = 4,
          end = 8
        ),
        year = str_sub(
          Variable,
          start = 10,
          end = 13
        ),
        estimacion = estimacion,
        .keep = "none"
      ) %>%
      mutate.( # TODO : Mejorar esto desde la receta
        variable = case_when.(
          variable == "B.1_1" ~ "IDinterna",
          variable == "B.1_2" ~ "IDexterna",
          variable == "B.1_3" ~ "bienescapital", # nolint
          variable == "B.1_4" ~ "software", # nolint
          variable == "B.1_5" ~ "propiedadintelectual", # nolint
          variable == "B.1_6" ~ "ingenieria", # nolint
          variable == "B.1_7" ~ "capactiacion", # nolint
          variable == "B.1_8" ~ "marketing", # nolint
          variable == "B.1_9" ~ "gestion"
        )
      ) %>%
      summarise.(
        estimacion = sum(estimacion),
        .by = "variable"
      ) %>%
      assign(
        "result",
        .,
        envir = parent.env(
          environment()
        )
      )
  }

  return(
    result
  )
}
