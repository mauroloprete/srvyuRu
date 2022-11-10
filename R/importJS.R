#' @export
get_id <- function() {
  includeScript(
    system.file(
      "js/get_id.js",
      package = "srvyuRu"
    )
  )
}
