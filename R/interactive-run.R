#' Interactive
#' @name run_interactive.
#' @rdname run_interactive.
#' @keywords easy
#' @export

run_interactive. <- function() { # nolint



  runGadget(
    app_ui,
    app_server,
    viewer = paneViewer(minHeight = "maximize")
  )
  # viewer = dialogViewer(
  #   "srvyuRu : Procesamiento interactivo {COGNUS}",
  #   width = 1200,
  #   height = 1500
  # )
}
