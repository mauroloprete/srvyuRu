buttonValue <- function( # nolint
                        id,
                        label) {
  actionButton(
    id,
    label,
    onclick = "Shiny.setInputValue('btnLabel', this.innerText);",
    style = "background-color:#40555D;
                      color:#FFFFFF;
                      border-color:#40555D;
                      border-style:double;
                      border-width:2px;
                      border-radius:0%;
                      font-size:17px;"
  )
}

buttonIcon <- function( # nolint
                       id,
                       icon,
                       label) {
  actionButton(
    id,
    icon,
    label,
    style = "background-color:#40555D;
                      color:#FFFFFF;
                      border-color:#40555D;
                      border-style:double;
                      border-width:2px;
                      border-radius:0%;
                      font-size:17px;"
  )
}

list_svy <- function(.data) {
  tidytable(
    ponderador = get_weight.(.data),
    edicion = get_edition.(.data),
    svy_type = get_svy_type.(.data),
  )
}

list_metadata <- function(.data) {
  tidytable(
    class = class(.data),
    tasa_no_nulo = round(
      (mean(!is.na(.data))),
      1
    )
  )
}


headerUI <- function(title) { # nolint
  f7Shadow(
    f7Card(
      f7Row(
        f7BlockTitle(
          f7Align(
            h4(title), # nolint
            side = "center"
          ),
          size = "medium"
        ),
        f7Align(
          img(
            src = "https://cognus.gitlab.io/proyectos/anii/srvyuru/logo.png", # nolint
            width = "10%"
          ),
          side = "center"
        )
      )
    ),
    intensity = 10,
    hover = TRUE,
    pressed = TRUE
  )
}
