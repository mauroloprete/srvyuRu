load.popupUI <- function(id,
                         title) {

  # TODO : Poner diccionarios!

  f7Popup(
    id = NS(
      id,
      "popup"
    ),
    title = h2(title),
    f7Col(
      f7Card(
        title = h5("Edición"),
        f7Text(
          NS(
            id,
            "edicion"
          ),
          value = "",
          label = NULL
        )
      ),
      f7Card(
        title = h5("Selecciona el archivo"),
        f7Row(
          f7File(
            NS(
              id,
              "file_sav"
            ),
            multiple = FALSE,
            buttonLabel = "Subir .sav",
            label = NULL,
            placeholder = NULL,
            accept = ".sav",
            width = "45%"
          ),
          f7File(
            NS(
              id,
              "file_rdata"
            ),
            multiple = FALSE,
            buttonLabel = "Subir .RData",
            label = NULL,
            placeholder = NULL,
            accept = ".RData",
            width = "45%"
          )
        )
      ),
      f7Card(
        title = h5("Ponderador"),
        uiOutput(
          NS(
            id,
            "colnames"
          )
        )
      ),
      f7Card(
        title = h5("Metadata"),
        gt_output(
          NS(
            id,
            "metadata"
          )
        )
      ),
      f7Button(
        NS(
          id,
          "save"
        ),
        "Guardar",
        fill = FALSE,
        outline = TRUE
      )
    ),
    closeOnEscape = FALSE,
    swipeToClose = FALSE,
    closeByBackdropClick = TRUE
  )
}


load.popupServer <- function(id,
                             svy_type,
                             button) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(
        button(),
        {
          # Se abre el popup

          updateF7Popup(
            id = "popup"
          )


          # * Carga del archivo, se guarda en un objeto cada vez                                                                                                                                                                        # * que se cambia de estado (carga de archivos)

          reactive({ # nolint
            validate(
              need(
                input$file_sav,
                message = FALSE
              )
            )
            input$file_sav
          }) %>%
            assign(
              "userFile",
              .,
              envir = parent.env(
                environment()
              )
            )

          # * Lectura y carga de datos al environment

          reactive({ # nolint

            req(
              input$edicion
            )

            if (svy_type == "eaii" && (input$edicion != "Indicar una edición" | is.null(input$edicion))) { # nolint

              userFile() %$%
                datapath %>%
                load_base.(
                  .dir = .
                ) %>%
                set_weight.(
                  .eaii = input$edicion
                ) %>%
                set_attr(
                  "edicion",
                  input$edicion
                ) %>%
                set_attr(
                  "survey",
                  svy_type
                )
            } else {
              if (
                svy_type == "ech" && # nolint
                  (input$edicion != "Indicar una edición" | is.null(input$edicion)) # nolint
              ) { # nolint

                userFile() %$%
                  datapath %>%
                  load_base.(
                    .dir = .
                  ) %>%
                  set_weight.(
                    .ech = input$edicion
                  ) %>%
                  set_attr(
                    "edicion",
                    input$edicion
                  ) %>%
                  set_attr(
                    "survey",
                    svy_type
                  )
              } else {
                userFile() %$%
                  datapath %>%
                  load_base.(
                    .dir = .
                  ) %>%
                  set_weight.(
                    .weight = input$edicion
                  ) %>%
                  set_attr(
                    "edicion",
                    input$edicion
                  ) %>%
                  set_attr(
                    "survey",
                    svy_type
                  )
              }
            }
          }) %>%
            assign(
              "svy",
              .,
              envir = parent.env(
                environment()
              )
            )


          output$metadata <- render_gt(
            { # nolint

              svy() %>%
                lapply(
                  X = .,
                  FUN = list_metadata
                ) %>%
                bind_rows.(
                  .id = "Variable"
                ) %>%
                set_names(
                  c(
                    "Variable",
                    "Tipo",
                    "Datos no nulos"
                  )
                ) %>%
                mutate.(
                  Variable = tolower(
                    Variable
                  )
                ) %>%
                gt() %>%
                fmt_percent(
                  column = `Datos no nulos`,
                  decimals = 1
                ) %>%
                tab_style(
                  locations = cells_column_labels(
                    columns = everything()
                  ),
                  style = list(
                    cell_fill(color = "#40555D"),
                    cell_text(
                      weight = "bold",
                      color = "#FFFFFF"
                    )
                  )
                ) %>%
                tab_style(
                  locations = cells_body(
                    Variable
                  ),
                  style = list(
                    cell_text(
                      weight = "bold"
                    )
                  )
                ) %>%
                opt_table_font(
                  font = list(
                    google_font(
                      name = "Merriweather"
                    ),
                    "Cochin",
                    "Serif"
                  )
                )
            },
            width = "100%",
            height = px(200)
          )

          # * Selector reactivo a edición de la encuesta

          output$colnames <- renderUI({ # nolint
            f7Select(
              "ponderador",
              label = NULL,
              selected = get_weight.(
                svy()
              ),
              choices = c(
                "Elegir ponderador o indicar edición correcta", # nolint
                names(
                  svy()
                )
              )
            )
          })


          observeEvent(
            input$save,
            { # nolint

              svy() %>%
                assign(
                  paste(
                    svy_type,
                    str_replace(
                      attributes(.)$edicion,
                      "-",
                      "_"
                    ),
                    sep = "_"
                  ),
                  .,
                  envir = svy_env
                )

              # * Se dejan como predeterminados todos los input

              # * input de edición

              updateF7Text(
                "edicion",
                value = ""
              )

              # * Mensaje de aviso

              f7Notif(
                text = as.character(
                  glue(
                    "Se cargo la encuesta al ambiente de trabajo con el nombre {svy_name}", # nolint
                    svy_name = paste(
                      svy_type,
                      str_replace(
                        attributes(
                          svy()
                        )$edicion,
                        "-",
                        "_"
                      ),
                      sep = "_"
                    )
                  )
                ),
                icon = f7Icon(
                  "checkmark_2"
                ),
                title = "Encuesta cargada",
                titleRightText = "Ahora",
                closeButton = TRUE,
                closeTimeout = 5000
              )
            }
          )
        }
      )
    }
  )
}
