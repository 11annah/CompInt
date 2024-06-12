# For seperate package, the function check_model_class may also be needed.

get_original_formula <- function(model) {
  # extract formula from standard model objects
  formula_standard <- try(model[["formula"]], silent = TRUE)
  # s4 objects need to be extracted with `@` only
  formula_s4 <- try(model@formula, silent = TRUE)

  # Use formula embedded in model objects if possible. Otherwise, try
  # to call formula() and notify user with warning()
  formula <- if (!inherits(formula_standard, "try-error")) {
    formula_standard
  } else if (!inherits(formula_s4, "try-error")) {
    formula_s4
  } else {
    warning("Cannot extract formula from model object. Using formula() to extract value.")
    formula(model)
  }
  return(formula)
}

list_interaction <- function(notation, term_attr) {
  list(notation = notation, terms = term_attr$term.labels[which(grepl(notation, term_attr$term.labels))], present = any(grepl(notation, term_attr$term.labels)))
}

check_formula_validity <- function(regnames, data) {
  if (!all(regnames %in% names(data))) {
    stop("All terms used in the model formula need to correspond to variable names of the specified model object's data. Please fully preprocess the data before fitting the model.")
  }
}


make_modfam <- function(family, link) {
  list(Family = family, Link = link)
}
