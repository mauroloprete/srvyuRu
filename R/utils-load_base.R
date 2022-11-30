#' Crear un grupo para luego particionar la poblaci'on y calcular el total poblacional en cada momento
#' @name load_base.
#' @rdname load_base.
#' @keywords utils
#' @param .eaii Serie historica con el formato : AñoInicial-AñoFinal.
#' @param .dir En el caso que se quiera cargar un archivo de SPSS de forma local se le debe de indicar la ruta.
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


load_base. <- function(.type = NULL,
                       .ed = NULL,
                       .dir = NULL) {
  if (!(is.null(.type) && is.null(.ed))) {
    load_base.rdata(
      type = .type,
      ed = .ed
    ) %>%
      assign(
        "svy",
        .,
        envir = parent.env(
          environment()
        )
      )
  }


  if (!is.null(.dir)) {
    glue(
      "

      load_base.{ext}(
        .dir = '{.dir}'
      )

      ",
      ext = xfun::file_ext(.dir)
    ) %>%
      glue_eval() %>%
      assign(
        "svy",
        .,
        envir = parent.env(
          environment()
        )
      )
  }


  svy %>%
    as_tidytable()
}



load_base.rdata <- function(type,
                            ed) {
  glue(
    "
    get(
      load(
        here::here(
          'data',
          '{type}_{ed}.RData'
        )
      )
    ) %>%
    haven::zap_labels()
    "
  ) %>%
    parse_expr() %>%
    eval_tidy()
}


load_base.csv <- function(.dir) {
  data.table::fread(
    file = .dir
  )
}

load_base.sav <- function(.dir) {
  haven::read_sav(
    here::here(
      .dir
    ),
    encoding = "latin1"
  ) %>%
  haven::zap_labels()
}

