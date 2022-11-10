svy_environmentUI <- function( # nolint
                              id) {
  f7Card(
    f7Row(
      f7Block(
        f7Align(
          h2("Ambiente de trabajo"), # nolint
          side = "center"
        )
      ),
      div(
        style = "display:inline-block; float:right",
        actionButton(
          inputId = NS(
            id,
            "refresh"
          ),
          label = f7Block(
            "Actualizar",
            f7Icon(
              "memories_badge_plus"
            )
          ),
          fill = FALSE,
          onclick = "get_id(this.id)",
          style = "background-color: #FFFFFF;
                        color: #40555D;
                        padding: 7px 10px;
                        border: 2px solid #40555D;
                        font-size: 12px;
                        transition: 0.3s;
                        border-radius: 10px;
                        font-family: Arial;
                        font-weight: 600;"
        )
      )
    ),
    DT::DTOutput(
      outputId = NS(
        id,
        "table"
      ),
      height = "100%"
    ),
    environment_viewUI(
      NS(
        id,
        "popup_view"
      )
    ),
    environment_modifyUI(
      NS(
        id,
        "popup_modify"
      )
    ),
    environment_diccUI(
      NS(
        id,
        "popup_dicc"
      )
    ),
    footer = f7Link(
      "Para mas información, visitar aquí",
      "https://cognus.gitlab.io/proyectos/anii/srvyuru/" # TODO : Cambiar link y poner una vignette # nolint
    ),
    height = "700"
  )
}

svy_environmentServer <- function( # nolint
                                  id) {
  moduleServer(
    id,
    function(input, output, session) {

      # Row Buttons : Función para generar botones interactivos.
      # Como argumentos tiene el id, el prefijo , n la cantidad de encuestas. # nolint
      #  El prefijo es para identificar el tipo de botón.
      #  El namespace es para identificar el botón al usar Módulos.
      #  Se le debe de indicar el icono.

      rowButtons <- function( # nolint
                             id,
                             n,
                             prefix,
                             icon,
                             ns = NS(id)) {
        map_chr(
          .x = seq_len(n),
          .f = function(i) {
            as.character(
              glue(
                "{button_id}{namespace} {type} \"button\" {onclick}onclick=get_id(this.id) {class}\"button f7-action-button\"> <i {class}\"icon f7-icons\">{icon}</i> </button>", # nolint
                button_id = "<button id =",
                type = "type=",
                class = "class=",
                onclick = "onclick=",
                icon = icon,
                id = id,
                namespace = ns(
                  paste0(
                    prefix,
                    i
                  )
                )
              )
            )
          }
        )
      }

      observeEvent(
        input$refresh,
        { # nolint
          if (
            !identical(
              ls(
                envir = svy_env
              ),
              character(0)
            )
          ) {
            # Se genera tabla en base a los elementos que están en svy_env # nolint


            table_environment <- reactive(
              eapply(
                svy_env,
                FUN = list_svy
              ) %>%
                bind_rows.(
                  .id = "Encuesta"
                ) %>%
                mutate.(
                  modificar = rowButtons(
                    id = id,
                    n = nrow(.),
                    prefix = "modify_",
                    icon = "wand_rays_inverse"
                  ),
                  view = rowButtons(
                    id = id,
                    n = nrow(.),
                    prefix = "view_",
                    icon = "zoom_in"
                  ),
                  metadata = rowButtons(
                    id = id,
                    n = nrow(.),
                    prefix = "dicc_",
                    icon = "book"
                  ),
                  exportar = rowButtons(
                    id = id,
                    n = nrow(.),
                    prefix = "export_",
                    icon = "square_arrow_down_on_square"
                  ),
                  delete = rowButtons(
                    id = id,
                    n = nrow(.),
                    prefix = "del_",
                    icon = "multiply_circle"
                  )
                ) %>%
                set_names(
                  c(
                    "Encuesta",
                    "Ponderador",
                    "Edición",
                    "Tipo de Encuesta",
                    "Modificar",
                    "Vista previa",
                    "Metadata",
                    "Exportar",
                    "Eliminar"
                  )
                )
            )


            output$table <- DT::renderDataTable(
              { # nolint
                table_environment()
              },
              escape = FALSE,
              selection = "none",
              options = list(
                dom = "t",
                columnDefs = list(
                  list(
                    className = "dt-center",
                    targets = "_all"
                  )
                ),
                initComplete = DT::JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#40555D', 'color': '#fff'});", # nolint
                  "}"
                ),
                ordering = FALSE
              ),
              rownames = FALSE,
              class = "display dt-responsive cell-border compact", # nolint
              width = "100%",
              height = "100%"
            )
          } else {

            # Mensaje de Alerta cuando no hay encuestas.

            f7Dialog(
              id = NULL,
              title = "¡No hay encuestas cargadas!",
              f7Block(
                f7Align(
                  h4(
                    "Para cargar una, haga click en el botón \"Cargar encuesta\"." # nolint
                  ),
                  side = "justify"
                ),
              ),
              type = "alert",
              session = shiny::getDefaultReactiveDomain()
            )

            output$table <- NULL
          }
        }
      )



      # ! Popups de los actionButton ------------------

      # * ---------------------------------------------

      # ! Botón de View -------------------------------
      environment_viewServer(
        id = "popup_view",
        current_id = reactive(
          input$current_id
        )
      )

      # ! Botón de Modificar --------------------------
      environment_modifyServer(
        id = "popup_modify",
        current_id = reactive(
          input$current_id
        )
      )

      # ! Botón de Metadata ---------------------------
      environment_diccServer(
        id = "popup_dicc",
        current_id = reactive(
          input$current_id
        )
      )

      # ! Botón de Exportar ---------------------------
      environment_exportServer(
        id = "dialog_delete",
        current_id = reactive(
          input$current_id
        )
      )

      # ! Botón de Eliminar ---------------------------
      environment_deleteServer(
        id = "dialog_delete",
        current_id = reactive(
          input$current_id
        )
      )

      # * ---------------------------------------------
    }
  )
}
