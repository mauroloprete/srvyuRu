#' Set a weight
#' @name set_weight.
#' @rdname set_weight.
#' @keywords utils
#' @param .data A tibble.
#' @param .name Nombre de la variable que vamos a crear.
#' @param .keep keep variabels
#' @return Devuelve un tibble.
#'
#'
#' @export
set_weight. <- function(.data,
                        .eaii = NULL,
                        .ech = NULL,
                        .weight = weight) {
  if (is.null(.eaii)) {
    if (!is.null(.ech)) {
      if (as.numeric(.ech) <= 2019) {
        .data %<>%
          set_attr(
            "weight",
            "pesoano"
          )
      } else {
        if (as.numeric(.ech) == 2021) {
          .data %<>%
            set_attr(
              "weight",
              "w_anual"
            )
        } else {
          if ("pesomen" %in% names(.data)) {
            .data %<>%
              compute(
                pesoano,
                pesomen / 12
              ) %>%
              set_attr(
                "weight",
                "pesoano"
              )
          } else {
            stop("ECH no válida, fije el ponderador manualmente")
          }
        }
      }
    } else {
      .data %<>%
        set_attr(
          "weight",
          .weight
        )
    }
  } else {
    if (.eaii == "1998-2000") {
      .data %<>%
        set_attr(
          "weight",
          "peso"
        )
    }

    if (.eaii == "2001-2003") {
      .data %<>%
        set_attr(
          "weight",
          "EXPANSOR"
        )
    }

    if (.eaii == "2004-2006") {
      .data %<>%
        set_attr(
          "weight",
          "expansor"
        )
    }

    if (.eaii == "2007-2009") {
      .data %<>%
        set_attr(
          "weight",
          "expansor"
        )
    }

    if (.eaii == "2010-2012") {
      .data %<>%
        set_attr(
          "weight",
          "peso"
        )
    }

    if (.eaii == "2013-2015") {
      .data %<>%
        set_attr(
          "weight",
          "w_transver"
        )
    }

    if (.eaii == "2016-2018") {
      .data %<>%
        set_attr(
          "weight",
          "peso.cs"
        )
    }
  }

  message(
    crayon::green(
      glue::glue(
        "Se ha fijado el ponderador en los datos. Puede usar get_weight.() para consultar" # nolint
      )
    )
  )

  return(.data)
}


#' Get a weight
#' @name get_weight.
#' @rdname get_weight.
#' @keywords utils
#' @param .data A tibble.
#' @return Devuelve un tibble.
#' @import glue
#' @export
#'
#'

get_weight. <- function(.data) {
  if (
    !is.null(attributes(.data)$weight)
  ) {
    .data %>%
      attributes() %$%
      weight
  } else {
    glue(
      "No se ha fijado el ponderador"
    )
  }
}

get_edition. <- function(.data) {
  if (
    !is.null(attributes(.data)$edicion)
  ) {
    .data %>%
      attributes() %$%
      edicion
  } else {
    glue(
      "No se ha fijado la edición"
    )
  }
}

get_svy_type. <- function(.data) {
  if (
    !is.null(attributes(.data)$survey)
  ) {
    .data %>%
      attributes() %$%
      survey
  } else {
    glue(
      "No se reconoce el tipo de encuesta"
    )
  }
}
