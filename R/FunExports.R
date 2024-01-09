#' @export
gsub_complex <- function(expr, string) {
  gsub(gsub("([][\\^$.|?*+()])", "\\\\\\1", expr, perl = TRUE), "", string)
}
