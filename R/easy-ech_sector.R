# Función para hacer públicación de mercado de trabajo

# TODO : VER NOTA DE REUNIÓN CON FRANCO


to_sector. <- function( # nolint
                       .data,
                       .ech,
                       .ciiu3 = NULL,
                       .ID = NULL # nolint
) {
  if (
    as.numeric({{ .ech }}) < 2009
  ) {
    .data %>%
      to_ciiu4.(
        .ciiu3 = {{ .ciiu3 }},
        .ID = {{ .ID }}
      )
  } else {
    .data %>%
      left_join.(
        oslo
      )
  }
}
