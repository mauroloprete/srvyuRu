#' Recetas de la ECH
#' @docType data
#' @keywords recipes
#' @keywords ech
#' @name ech_recipes
#' @usage recipes
#' @format Correspondencia de CIIU revisión 3 a revisión 4
"recipes"

#' Correspondencia de CIIU revisión 3 a revisión 4
#' @docType data
#' @keywords ciiu
#' @name correspondencia
#' @usage correspondencia
#' @format Correspondencia de CIIU revisión 3 a revisión 4
"correspondencia"

#' Cotizaciones del BCU
#' @docType data
#' @keywords data_aux
#' @name cotizaciones_bcu
#' @usage cotizaciones_bcu
#' @format Cotizaciones del BCU
"cotizaciones_bcu"

#' Cotinzaciones del INE anuales
#' @docType data
#' @keywords data_aux
#' @name cotizaciones_ine_anual
#' @usage cotizaciones_ine_anual
#' @format Recetas de la ech
"cotizaciones_ine_anual"

#' Cotinzaciones del INE mensuales
#' @docType data
#' @keywords data_aux
#' @name cotizaciones_ine_mensual
#' @usage cotizaciones_ine_mensual
#' @format Recetas de la ech
"cotizaciones_ine_mensual"


# recipes


recipes <- openxlsx::read.xlsx(here::here("recipes.xlsx"))



# load_base.(
#     .type = "ech",
#     .ed = "2019"
# ) %>%
# load_recipes_ech.(
#     .ech = "2019",
#     .recipe = "genre"
# ) %>%
# select.(
#     genero,
#     edad
# )


# recipes %>%
#     filter.(
#         edicion == 2019,
#         tipo == "genre"
#     )

# # recipes %>%
# #     filter.(
# #         edicion == "2019",
# #         tipo == "genre"
# #     ) %>%
# #     pull.(receta)

