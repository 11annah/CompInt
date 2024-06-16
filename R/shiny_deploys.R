#' @export
find_my_method <- function() {
  shiny::runApp(system.file("shiny", "decisiontree_app", package = "CompInt"))
}
