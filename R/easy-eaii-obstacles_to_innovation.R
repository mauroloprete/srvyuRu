#' Estimate the ratio obstacle to innovation
#' @name estimate_ratio_obstacle_innovation.
#' @rdname estimate_ratio_obstacle_innovation.
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
#'   estimate_ratio_obstacle_innovation.(
#'     .eaii = "2016-2018"
#'   )
#' @return Returns a tibble with the created variables
#'
#' @export




estimate_ratio_obstacle_innovation. <- function( # nolint
                                                .data,
                                                .eaii,
                                                .by = "realiza_innovacion",
                                                .recipe = "obstaculos",
                                                .vars = c(
                                                  "personal_capacitado",
                                                  "rigidez",
                                                  "riesgos",
                                                  "retorno",
                                                  "tamanio_mercado",
                                                  "oportunidades_tecno",
                                                  "financiamiento",
                                                  "cooperacion",
                                                  "insuficiencia_mercados",
                                                  "insuficiencia_tecno",
                                                  "desarrollo_instituciones",
                                                  "infraestructura",
                                                  "prop_deficiente",
                                                  "inestabilidad_macro",
                                                  "otros"
                                                )) { # nolint


  .data %<>%
    set_weight.(
      .eaii = {{ .eaii }}
    ) %>%
    load_recipes_eaii.(
      .eaii = {{ .eaii }},
      .recipe = {{ .recipe }}
    )


  .vars %>%
    map_dfr(
      .f = function(x) {
        .data %>%
          stats_ratio.(
            !!rlang::sym(
              paste0(
                "alta_",
                x
              )
            ),
            !!rlang::sym(
              paste0(
                "total_",
                x
              )
            ),
            .name = !!rlang::sym(x),
            .by = {{ .by }}
          )
      }
    ) %>%
    recode(
      innovativa,
      realiza_innovacion == 1 ~ "Si",
      realiza_innovacion == 0 ~ "No",
      .keep = "unused"
    ) %>%
    recode(
      grupo,
      Variable == "personal_capacitado" ~ "Microeconomicas o empresariales",
      Variable == "rigidez" ~ "Microeconomicas o empresariales",
      Variable == "riesgos" ~ "Microeconomicas o empresariales",
      Variable == "retorno" ~ "Microeconomicas o empresariales",
      Variable == "tamanio_mercado" ~ "Mesoeconomicos o de mercado",
      Variable == "oportunidades_tecno" ~ "Mesoeconomicos o de mercado",
      Variable == "financiamiento" ~ "Mesoeconomicos o de mercado",
      Variable == "cooperacion" ~ "Mesoeconomicos o de mercado",
      TRUE ~ "Macroeconomicas"
    ) %>%
    recode(
      categoria,
      Variable == "personal_capacitado" ~ "Escasez de personal capacitado",
      Variable == "rigidez" ~ "Rigidez organizacional",
      Variable == "riesgos" ~ "Riesgos que implica la innovación",
      Variable == "retorno" ~ "Periodo de retorno de la inversión",
      Variable == "tamanio_mercado" ~ "Reducido tamaño del mercado",
      Variable == "oportunidades_tecno" ~ "Escasas oportunidades tecnológicas del sector al que pertenece la empresa", # nolint
      Variable == "financiamiento" ~ "Dificultades de acceso al financiamiento", # nolint
      Variable == "cooperacion" ~ "Escasas posibilidades de cooperación con otras empresas / instituciones", # nolint
      Variable == "insuficiencia_mercados" ~ "Escasas posibilidades de cooperación con otras empresas / instituciones", # nolint
      Variable == "insuficiencia_tecno" ~ "Insuficiente información sobre tecnologías", # nolint
      Variable == "desarrollo_instituciones" ~ "Escaso desarrollo de instituciones relacionadas con Ciencia / Tecnología", # nolint
      Variable == "infraestructura" ~ "Infraestructura física inadecuada", # nolint
      Variable == "prop_deficiente" ~ "Sistema de propiedad intelectual deficiente", # nolint
      Variable == "inestabilidad_macro" ~ "Inestabilidad macroeconómica", # nolint
      Variable == "otros" ~ "Otros obstáculos", # nolint
      .keep = "unused"
    ) %>%
    compute(
      estimacion,
      round(
        estimacion,
        0
      )
    )


  #  %>%
  # pivot_wider.(
  #     names_from = innovativa,
  #     values_from = estimacion
  # ) %>%
  # relocate.(
  #     categoria,
  #     grupo,
  #     Si,
  #     No
  # )
}
