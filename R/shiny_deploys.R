#' @export

find_my_method <- function(){
  httpuv::runStaticServer(system.file("shiny","decisiontree_app","shinydocs",package="CompInt"))
}



