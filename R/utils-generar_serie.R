get_series. <- function(srvy_name = NULL,
                        load = FALSE,
                        type,
                        easy_fun,
                        ...) {
  if (load) {
    map2_dfr.(
      .x = srvy_name,
      .y = names(srvy_name),
      .f = function(x, y) {
        glue(
          "
                    load_base.(
                        .dir = here::here(x)
                    ) %>%
                    estimate_{easy_fun}.(
                        .{type} = '{y}',
                        ...
                    ) %>%
                    mutate.(
                        year = {y}
                    )

                    "
        ) %>%
          parse_expr() %>%
          eval_tidy()
      }
    )
  } else {
    print("Utilizando datos en memoria")

    map2_dfr.(
      .x = srvy_name,
      .y = names(srvy_name),
      .f = function(x, y) {
        glue(
          "
                    {x}  %>%
                         estimate_{easy_fun}.(
                             .{type} = '{y}',
                             ...
                         )
                    "
        ) %>%
          parse_expr() %>%
          eval_tidy()
      }
    )
  }
}
