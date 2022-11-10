#' Realizar estimaciones a nivel de persona para la ECH
#' @name to_personas.
#' @rdname to_personas.
#' @keywords ech
#' @keywords utils
#' @param .data A tibble.
#' @param .ech
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


to_personas. <- function(.data,.ech) {
    
    if (as.numeric({{.ech}}) < 2020) {
        message(
            glue::glue(
                crayon::green(
                "La edición de la {.ech} en su versión pública viene a nivel de personas en un archivo aparte. No es necesario realizar cambios"
                )
            )
        )
        .data
    } else {
        .data %>%
            filter.(
                hogar == 0
            )
    }
        
}
