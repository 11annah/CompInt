#' @export
produce_model_definition <- function(mod,
                                     separate_interactions = FALSE,
                                     name_targetvar = NULL) {
  # Insert stop message
  LinPred <- make_linear_predictor(
    mod = mod, reg_of_interest = NULL, separate_interactions = separate_interactions
  )
  Y <- name_targetvar %||% "Y"
  # TOFIX
  indexing <- "[l]"
  model_formula <- sprintf(
    "E[%s|(%s)] = %s",
    Y,
    toString(unlist(LinPred$reg_groups)),
    gsub(
      gsub("([][\\^$.|?*+()])", "\\\\\\1", indexing, perl = TRUE),
      "",
      LinPred$non_vectorized
    )
  )
  # EXTEND
  output_plain <- list(model_formula = model_formula)
  output_plain
}

#' @export
regs <- function(object) {
  check_model_class(object, "object")
  unlist(object[["model_specification"]][["regs"]][c("metric", "categorical")])
}

#' @export
gsub_complex <- function(expr, string) {
  gsub(gsub("([][\\^$.|?*+()])", "\\\\\\1", expr, perl = TRUE), "", string)
}

#' @export
list_contains <- function(list1, list2) {
  all(sapply(list1, function(x) any(sapply(list2, function(y) all(x %in% y)))))
}
