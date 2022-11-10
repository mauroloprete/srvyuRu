environment_modify_load_recipeUI <- function(id) {
  f7Tab(
    tabName = "Aplicar recta",
    f7Card(
      h4(
        textOutput(
          NS(
            id,
            "title"
          )
        )
      ),
      f7Col(
        uiOutput(
          NS(
            id,
            "select"
          )
        ),
        actionButton(
          inputId = NS(
            id,
            "apply_recipe"
          ),
          label = f7Block(
            "Actualizar",
            f7Icon(
              "memories_badge_plus"
            )
          ),
          fill = FALSE,
          onclick = "remove_id(this.id)",
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
    )
  )
}

environment_modify_load_recipeServer <- function(id,
                                                 current_id) {
  moduleServer(
    id,
    function(input,
             output,
             session) {

      # Se guarda temporalmente la encuesta. El nombre es un objeto reactivo, que cambia cada vez que se llama al
      # módulo.

      svy_name <- reactive(
        eapply(
          svy_env,
          FUN = list_svy
        ) %>%
          bind_rows.(
            .id = "Encuesta"
          ) %>%
          slice.(
            str_extract(
              current_id,
              pattern = "[0-9]+"
            ) %>%
              as.numeric()
          ) %>%
          pull.(
            Encuesta
          )
      )

      svy_name() %>%
        rlang::parse_expr() %>%
        rlang::eval_tidy(
          env = svy_env
        ) %>%
        assign(
          "svy",
          .,
          envir = parent.env(
            environment()
          )
        )


      .edition <- get_edition.(
        svy
      )


      .type <- get_svy_type.(
        svy
      )

      recipes %>%
        filter.(
          encuesta == .type,
          edicion == .edition
        ) %>%
        pull.(
          tipo
        ) %>%
        assign(
          "choices",
          .,
          envir = parent.env(
            environment()
          )
        )

      output$title <- renderText({
        paste(
          "Se encuentran disponibles las siguientes recetas para la ", # nolint
          toupper(
            .type
          ),
          " ",
          .edition,
          ":",
          sep = " "
        )
      })

      output$select <- renderUI({ # nolint
        ns <- session$ns # nolint
        f7Select(
          ns(
            "recipe"
          ),
          label = NULL,
          choices = c(
            choices,
            "Seleccionar receta"
          ),
          selected = "Seleccionar receta"
        )
      })

      observeEvent(
        input$apply_recipe,
        { # nolint
          if (
            input$recipe != "Seleccionar receta"
          ) {
            print(
              get_edition.(
                svy
              )
            )

            svy %>%
              load_recipes_ech.(
                .ech = .edition,
                .recipe = input$recipe
              ) %>%
              assign(
                svy_name(),
                .,
                envir = svy_env
              )

            f7Notif(
              text = as.character(
                glue(
                  "Se cargó la receta {recipe} a la {svy_name}", # nolint
                  svy_name = svy_name(),
                  recipe = input$recipe,
                )
              ),
              icon = f7Icon(
                "checkmark_2"
              ),
              title = "Encuesta actualizada",
              titleRightText = "Ahora",
              closeButton = TRUE,
              closeTimeout = 5000
            )

            updateF7Select(
              "recipe",
              selected = "Seleccionar receta"
            )
          }
        }
      )
    }
  )
}
