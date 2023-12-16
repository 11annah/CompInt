get_original_formula<-function(model){
  # extract formula from standard model objects
  formula_standard <- try(model[["formula"]], silent = TRUE)
  # s4 objects need to be extracted with `@` only
  formula_s4 <- try(model@formula, silent = TRUE)

  # Use formula embedded in model objects if possible. Otherwise, try
  # to call formula() and notify user with warning()
  formula <- if (!inherits(formula_standard, "try-error")) {
    formula_standard
  } else if (!inherits(formula_s4, "try-error")){
    formula_s4
  } else {
    warning("Cannot extract formula from model object. Using formula() to extract value.")
    formula(model)
  }
  return(formula)
}

