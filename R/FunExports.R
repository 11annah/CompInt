#' #TOFIX
#'
#' @export


produce_model_definition <- function(mod, separate_interactions = FALSE, name_targetvar = NULL) {
  # Insert stop message
  LinPred <- make_linear_predictor(mod = mod, reg_of_interest = NULL, separate_interactions = separate_interactions)
  Y <- ifelse(is.null(name_targetvar), "Y", name_targetvar)
  # TOFIX
  indexing <- "[l]"
  model_formula <- paste0(
    "E[", Y, "|(", paste(unlist(LinPred$reg_groups), collapse = ", "), ")] = ", gsub(gsub("([][\\^$.|?*+()])", "\\\\\\1", indexing, perl = TRUE), "", LinPred$non_vectorized)
  )
  output_plain <- list(model_formula = model_formula)
  # EXTEND
  return(output_plain)
}

#' @export
regs <- function(object) {
  check_model_class(object, "object")
  return(unlist(object[["model_specification"]][["regs"]][c("metric", "categorical")]))
}


#' @export
gsub_complex <- function(expr, string) {
  gsub(gsub("([][\\^$.|?*+()])", "\\\\\\1", expr, perl = TRUE), "", string)
}

#' @export
list_contains <- function(list1, list2) {
  all(sapply(list1, function(x) any(sapply(list2, function(y) all(x %in% y)))))
}
