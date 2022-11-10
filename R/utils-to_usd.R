#' Realizar estimaciones a nivel de persona para la ECH
#' @name to_usd.
#' @rdname to_usd.
#' @keywords utils
#' @param .data A tibble.
#' @param .var je
#' @param .year jm
#' @param type tipo de cotización
#' @examples
#' load_base.(
#'   "2016-2018"
#' ) %>%
#'   recode(
#'     var1,
#'    IB_3.1.1 == 'pri' ~ 'Privada',
#'     TRUE ~ 'Publica/Mixta'
#'     .ncond = "',"
#'   ) %>%
#'   recode(
#'     var2,
#'    RAI_E.1_1 == 1 ~ 'Si',
#'    RAI_E.1_2 == 1 ~ 'Si',
#'    TRUE ~ 'No',
#'    .keep = 'none' # Con keep = 'none' nos quedamos solo con las varaibles que creamos
#'   ) %>%
#'   tidytable::select.(
#'     var1,
#'     var2
#'   )
#' @return Devuelve un tibble.
#' @export
to_usd. <- function(.data, .var, .year, type = "anii") {

    if (type == "anii") {

        cotizaciones_bcu %>%
            filter.(
                anio == {{.year}}
            ) %>%
            pull.(
                promedio_vta_usd_anii
            ) %>%
            assign(
                "cotizacion",
                .,
                envir = parent.env(environment())
            )

    } else {

        cotizaciones_bcu %>%
            filter.(
                anio == {{.year}}
            ) %>%
            pull.(
                promedio_venta_usd
            ) %>%
            assign(
                "cotizacion",
                .,
                envir = parent.env(environment())
            )
    }


    message(
        crayon::bgMagenta(
            glue::glue(
                "Pesos por dolar : {cotizacion} *fuente {type}*"
            )
        )
    )

    .data %>%
        mutate.(
            "{{.var}}_usd" := {{ .var }} / cotizacion
        )


}
