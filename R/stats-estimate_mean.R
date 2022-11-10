#' Estimate a mean from a sample of data with H-T
#' @name estimate_proportion
#' @rdname estimate_proportion
#' @keywords stats
#' @param .variable variable
#' @param .weight weight
#' @param .by a variable to agroup the data
#' @param .filer_labels
#' @importFrom crayon red
#' @importFrom glue glue
#'
#' @export

stats_proportion. <- function( # nolint
                              .data,
                              .variable = NULL,
                              .by = NULL,
                              .filter_labels = NULL) { # nolint


  if (
    (!is.null(attributes(.data)$weight))
  ) {
    .data %>%
      attributes() %$%
      weight %>%
      sym() %>%
      assign(
        ".weight",
        .,
        envir = parent.env(
          environment()
        )
      )
  } else {
    stop(
      glue(
        red(
          "No se ha fijado el ponderador con set_weight. o se ha fijado uno incorrecto" # nolint
        )
      ) # nolint
    )
  }

  .data %<>%
    dummy_cols(
      {{ .variable }},
      omit_colname_prefix = FALSE
    ) %>%
    mutate.(
      across.(
        starts_with(
          "dummy_"
        ),
        ~ .x * !!.weight
      )
    ) %>%
    summarise.(
      across.(
        starts_with(
          "dummy_"
        ),
        ~ (
          sum(.x, na.rm = TRUE) / sum(!!.weight)
        ) * 100
      ),
      .by = {{ .by }}
    ) %>%
    pivot_longer.(
      -{{ .by }},
      names_to = "Variable",
      values_to = "Estimacion"
    ) %>%
    mutate.(
      id = map_chr.(
        .x = Variable,
        ~ tail(
          str_split(.x, "dummy_[[0-9][A-Z][a-z]]*_")[[1]],
          n = 1
        )
      )
    )

  if (!is.null({{ .by }})) {
    .data %<>%
      left_join.(
        dicc,
        by = c("id" = "variable")
      ) %>%
      mutate.(
        respuesta = str_extract(
          Variable,
          pattern = "[^dummy_]+"
        ),
        .keep = "unused"
      ) %>%
      magrittr::set_names(
        c(
          {{ .by }},
          "estimacion",
          "variable",
          "descripcion",
          "codigo",
          "respuesta"
        )
      ) %>%
      relocate.(
        variable,
        codigo,
        descripcion,
        respuesta,
        {{ .by }},
        estimacion
      )
  } else {
    .data %<>%
      left_join.(
        dicc,
        by = c("id" = "variable")
      ) %>%
      mutate.(
        respuesta = str_extract(
          Variable,
          pattern = "[^dummy_]+"
        ),
        .keep = "unused"
      ) %>%
      magrittr::set_names(
        c(
          "estimacion",
          "variable",
          "descripcion",
          "codigo",
          "respuesta"
        )
      ) %>%
      relocate.(
        variable,
        codigo,
        descripcion,
        respuesta,
        estimacion
      )
  }

  if (is.null(.filter_labels)) {
    .data %>%
      arrange.(
        variable,
        respuesta,
        desc(estimacion)
      )
  } else {
    .data %>%
      filter.(
        respuesta %in% {{ .filter_labels }}
      ) %>%
      arrange.(
        variable,
        respuesta,
        desc(estimacion)
      )
  }
}


#' Estimate a mean from a sample of data with H-T
#' @name estimate_mean
#' @rdname estimate_mean
#' @keywords stats
#' @param .vars
#' @param .weight weight
#' @param .by a variable to agroup the data
#' @importFrom crayon red
#' @importFrom glue glue
#'
#' @export


stats_mean. <- function( # nolint
                        .data,
                        .vars,
                        .by = NULL) {
  if (
    (!is.null(attributes(.data)$weight))
  ) {
    .data %>%
      attributes() %$%
      weight %>%
      sym() %>%
      assign(
        ".weight",
        .,
        envir = parent.env(
          environment()
        )
      )
  } else {
    stop(
      glue(
        red(
          "No se ha fijado el ponderador con set_weight. o se ha fijado uno incorrecto" # nolint
        )
      ) # nolint
    )
  }


  .data %<>%
    mutate.(
      across.(
        .cols = {{ .vars }},
        ~ .x * !!.weight
      )
    )

  if (!is.null({{ .by }})) {
    .data %<>%
      summarise.(
        across.(
          .cols = {{ .vars }},
          ~ sum(.x, na.rm = TRUE) / sum(!!.weight)
        ),
        .by = {{ .by }}
      ) %>%
      pivot_longer.(
        -{{ .by }},
        names_to = "Variable",
        values_to = "estimacion"
      ) %>%
      relocate.(
        Variable,
        {{ .by }},
        estimacion
      )
  } else {
    .data %<>%
      summarise.(
        across.(
          .cols = {{ .vars }},
          ~ sum(.x, na.rm = TRUE)
        )
      ) %>%
      pivot_longer.(
        names_to = "Variable",
        values_to = "estimacion"
      ) %>%
      relocate.(
        Variable,
        estimacion
      )
  }

  return(
    .data
  )
}
