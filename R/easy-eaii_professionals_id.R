#' Estimate the number of people engaged in I+D activities
#' @name estimate_professionals_id.
#' @rdname estimate_professionals_id.
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



estimate_professionals_id. <- function( # nolint
                                       .data,
                                       .eaii,
                                       .by = NULL,
                                       .recipe = "profesionales_id") { # nolint



  print(.recipe)

  if (.recipe == "profesionales_nivel") {
    .data %<>%
      load_recipes_eaii.(
        {{ .eaii }},
        {{ .recipe }}
      ) %>%
      set_weight.(
        .eaii = {{ .eaii }}
      ) %>%
      stats_totals.(
        c(
          "profesionales_grado",
          "profesionales_master",
          "profesionales_doctorado"
        )
      )
  } else {
    if (
      is.null(.by)
    ) {
      if (
        {{ .eaii }} == "2016-2018"
      ) {
        if (.recipe == "gender_area") {
          .data %<>%
            load_recipes_eaii.(
              {{ .eaii }},
              {{ .recipe }}
            ) %>%
            set_weight.(
              .eaii = .eaii
            ) %>%
            stats_totals.(
              c(
                "ciencias_naturales_mujeres",
                "ciencias_naturales_hombres",
                "ciencias_medicas_mujeres",
                "ciencias_medicas_hombres",
                "ingenieria_sistemas_electrica_mujeres",
                "ingenieria_sistemas_electrica_hombres",
                "ingenieria_quimica_mujeres",
                "ingenieria_quimica_hombres",
                "otras_ingenierias_mujeres",
                "otras_ingenierias_hombres",
                "ciencias_agricolas_mujeres",
                "ciencias_agricolas_hombres",
                "ciencias_sociales_mujeres",
                "ciencias_sociales_hombres",
                "admin_contabilidad_mujeres",
                "admin_contabilidad_hombres",
                "humanidades_mujeres",
                "humanidades_hombres",
                "id_ciencias_naturales_mujeres",
                "id_ciencias_naturales_hombres",
                "id_ciencias_medicas_mujeres",
                "id_ciencias_medicas_hombres",
                "id_ingenieria_sistemas_electrica_mujeres",
                "id_ingenieria_sistemas_electrica_hombres",
                "id_ingenieria_quimica_mujeres",
                "id_ingenieria_quimica_hombres",
                "id_otras_ingenierias_mujeres",
                "id_otras_ingenierias_hombres",
                "id_ciencias_agricolas_mujeres",
                "id_ciencias_agricolas_hombres",
                "id_ciencias_sociales_mujeres",
                "id_ciencias_sociales_hombres",
                "id_admin_contabilidad_mujeres",
                "id_admin_contabilidad_hombres",
                "id_humanidades_mujeres",
                "id_humanidades_hombres"
              ),
              .by = {{ .by }}
            )
        } else {
          .data %<>%
            load_recipes_eaii.(
              {{ .eaii }},
              {{ .recipe }}
            ) %>%
            set_weight.(
              .eaii = {{ .eaii }}
            ) %>%
            stats_totals.(
              c(
                "profesionales_totales",
                "profesionales_id",
                "profesionales_otros"
              )
            )
        }
      } else {
        .data %<>%
          load_recipes_eaii.(
            {{ .eaii }},
            {{ .recipe }}
          ) %>%
          set_weight.(
            .eaii = {{ .eaii }}
          ) %>%
          stats_totals.(
            profesionales_id
          )
      }
    } else {
      if (
        .by == "sexo"
      ) {
        .data %<>%
          load_recipes_eaii.(
            {{ .eaii }},
            {{ .recipe }}
          ) %>%
          set_weight.(
            .eaii = {{ .eaii }}
          ) %>%
          stats_totals.(
            c(
              "PTO_4.3_Total_M",
              "PTO_4.3_Total_H",
              "RH_C.2_Total_M",
              "RH_C.2_Total_H"
            )
          )
      } else {
        .data %<>%
          load_recipes_eaii.(
            {{ .eaii }},
            {{ .recipe }}
          ) %>%
          set_weight.(
            .eaii = {{ .eaii }}
          ) %>%
          stats_totals.(
            profesionales_id,
            {{ .by }}
          )
      }
    }
  }



  return(
    .data
  )
}
