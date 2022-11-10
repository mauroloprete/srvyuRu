#' Load and apply the recipe from a database table
#' @name new_recipe
#' @rdname new_recipe
#' @keywords utils
#' @param .type
#' @param .ed
#' @param .formula
#' @param ...
#' @examples
#'
#'
#'
#' load_base.(
#'   .dir = here::here(
#'     "data-raw",
#'     "2016-2018.sav"
#'   )
#' ) %>%
#'   load_recipes_from_db.(
#'     .cnn = example,
#'     .ed = 2016 - 2018,
#'     .recipe = "realiza_innovacion"
#'   )
#'
#' @return Returns a tibble with the created variables
#'
#' @export



new_recipe <- function(.type,.ed,.formula,...) {
  
  .formula <- enquo(.formula)

  dots <- list2(...)

  args <- pairlist2(
    .df = ,
    .type = ,
    .ed = 
  )

  expr <- glue::glue(
    ".df %>% {.formula}"
  )




  rlang::new_function(
    args,
    body = expr[2],
    env = .GlobalEnv
  )


}


#' Load and apply the recipe from a database table
#' @name load_recipes_from_db.
#' @rdname load_recipes_from_db.
#' @keywords utils
#' @param .data Tibble with the survey
#' @param .cnn Database connection
#' @param .ed Survey year
#' @param .recipe Recipe name
#' @examples
#'
#'
#'
#' load_base.(
#'   .dir = here::here(
#'     "data-raw",
#'     "2016-2018.sav"
#'   )
#' ) %>%
#'   load_recipes_from_db.(
#'     .cnn = example,
#'     .ed = 2016 - 2018,
#'     .recipe = "realiza_innovacion"
#'   )
#'
#' @return Returns a tibble with the created variables
#'
#' @export

load_recipes_from_db. <- function(.data, .cnn, .survey, .ed, .recipe) { # nolint

  message(
    crayon::green(
      glue::glue(
        "Cargando receta {.recipe} de la {.survey} edición {.ed}"
      )
    )
  )

  DBI::dbGetQuery(
    {{ .cnn }},
    "SELECT *
        FROM datamart_innovacion.aux_recetas
        WHERE edicion = $1 AND tipo = $2",
    params = c(
      {{ .ed }},
      {{ .recipe }}
    )
  ) %>%
    data.table::data.table() %>%
    pull.(
      receta
    ) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}

#' Load and apply the recipe from a database table to an ECH
#' @name load_recipes_ech.
#' @rdname load_recipes_ech.
#' @keywords utils
#' @param .data Tibble with the survey
#' @param .ed Survey year
#' @param .recipe Recipe name
#' @param .zap_labels Boolean of labels to be removed
#' @examples
#'
#'
#'
#' load_base.(
#'   .dir = here::here(
#'     "P_2019_Terceros.sav"
#'   )
#' ) %>%
#'   load_recipes_ech.(
#'     .ech = "2019",
#'     .recipe = "labor"
#'   )
#'
#' @return Returns a tibble with the created variables
#' @export load_recipes_ech.
#'

load_recipes_ech. <- function(.data, .ech, .recipe, zap_labels = TRUE) { # nolint

  message(
    crayon::green(
      glue::glue(
        "Se cargó la receta {.recipe} de la ECH edición {.ech}"
      )
    )
  )



  recipes %>%
    filter.(
      edicion == {{ .ech }},
      tipo == {{ .recipe }}
    ) %>%
    pull.(
      receta
    ) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}


#' Cargar recetas de la EAII
#' @name load_recipes_eaii.
#' @rdname load_recipes_eaii.
#' @keywords internal
#' @param .data Tibble con edición de la EAII.
#' @param .eaii Edición de la encuesta
#' @param .recipe Crear grupo de variables
#' @examples
#'
#' load_base.("2016-2018")
#'
#' load_base.(
#'   .dir = here::here(
#'     "data-raw",
#'     "2016-2018.sav"
#'   )
#' )
#' @return Devuelve un tibble.
#'
#' @export

# TODO : Poner receta que fije el sector dos dígitos en el mismo nombre para todas # nolint

# TODO : Pasar a base de datos antes de publicar

# TODO : Ver receta de apoyos públicos


