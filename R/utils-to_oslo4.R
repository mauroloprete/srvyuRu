#' Crear un grupo para luego particionar la poblaci'on y
#' calcular el total poblacional en cada momento
#' @name to_oslo4.
#' @rdname to_oslo4.
#' @keywords internal
#' @param .data A tibble.
#' @param .name Nombre de la variable que vamos a crear.
#' @param ... Condiciones para armar dicha variable
#' @param .keep 
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


to_oslo4. <- function(.data) {
    .data %>%
        mutate.(
        categoria = case_when.(
        categoria == "bienescapital" ~ "Adquisición de Bienes de Capital para innovación", # nolint|1
        categoria == "IDinterna" ~ "I+D interna",
        categoria == "IDexterna" ~ "I+D externa",
        categoria == "TICS" ~ "Adquisición de software y actividades de base de datos", # nolint
        categoria == "software" ~ "Adquisición de software y actividades de base de datos", # nolint
        categoria == "hardware" ~ "Adquisición de software y actividades de base de datos", # nolint
        categoria == "ingenieria" ~ "Diseño, ingeniería y otras actividades de trabajo creativo", # nolint
        categoria == "gestion" ~ "Actividades de gestión de la innovación",
        categoria == "capacitacion" ~ "Capacitación para innovación",
        categoria == "transferencia" ~ "Transferencias de Tecnología y Consultorías para innovación", # nolint
        categoria == "marketing" ~ "Actividades de marketing y de valor de marca", # nolint
        categoria == "propiedadintelectual" ~ "Actividades relacionadas con la propiedad intelectual" # nolint
        )
    )
}
