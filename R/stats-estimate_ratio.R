#' Estimate a ratio from a sample of data with H-T
#' @name stats_ratio.
#' @rdname stats_ratio.
#' @keywords stats
#' @param .num numerator
#' @param .den denominator
#' @param .name name of the ratio
#' @param .by a variable to group the data
#' @param .pivot
#'
#' @export
stats_ratio. <- function( # nolint
                         .data,
                         .num,
                         .den,
                         .name = ratio,
                         .by = NULL,
                         .pivot = TRUE) {
  if (
    (
      !is.null(attributes(.data)$weight)
    )
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
        .cols = c(
          {{ .num }},
          {{ .den }}
        ),
        ~ .x * !!.weight
      )
    ) %>%
    summarise.(
      across.(
        .cols = c(
          {{ .num }},
          {{ .den }}
        ),
        ~ sum(.x, na.rm = TRUE)
      ),
      .by = {{ .by }}
    ) %>%
    mutate.(
      "{{.name}}" := ({{ .num }} / {{ .den }}) * 100, # nolint
      .keep = "unused"
    )

  if (
    {{ .pivot }}) {
    if (
      !is.null({{ .by }})
    ) {
      .data %<>%
        pivot_longer.(
          -{{ .by }},
          names_to = "Variable",
          values_to = "estimacion"
        )
    } else {
      .data %<>%
        pivot_longer.(
          names_to = "Variable",
          values_to = "estimación"
        )
    }
  }

  return(
    .data
  )
}
