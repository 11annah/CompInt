#' Changing-model-structures: Functions for transforming specific model objects to CompInt model objects
#'
#' ... Complete documentation !!! #TOFIX
#'


#' @describeIn Changing-model-structures Transforms a standard glm model into a CompInt model
#' @export

glm_to_compint <- function(model) {
  original_formula <- get_original_formula(model)
  data <- model$data
  term_attr <- attributes(terms(model))

  check_formula_validity(names(term_attr$dataClasses), data)

  structure(
    list(
      model = model,
      data = model$data,
      catreg_list = NULL,
      formula = original_formula,
      type = "GLM",
      model_specification = list(
        family = make_modfam(family(model)[[1]], family(model)[[2]]),
        regs = list(
          categorical = intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses != "numeric")], term_attr$term.labels),
          metric = intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses == "numeric")], term_attr$term.labels),
          interactions = list_interaction(notation = ":", term_attr = term_attr)
        ),
        intercept = term_attr$intercept == 1
      ),
      inference = "frequentist",
      pseudo_posterior = list(
        args = list(coefs = coef(model), vcov = vcov(model)),
        fun = function(model, ndraws, ...) {
          MASS::mvrnorm(
            n = ndraws, mu = model[["pseudo_posterior"]][["args"]][["coefs"]],
            Sigma = model[["pseudo_posterior"]][["args"]][["vcov"]], ...
          )
        }
      )
    ),
    class = c("CompInt_model", "from glm")
  )
}


#' @describeIn Changing-model-structures Transforms a logistf model into a CompInt model
#' @export

logistf_to_compint <- function(model, data) {
  original_formula <- get_original_formula(model)
  term_attr <- attributes(terms(model))

  check_formula_validity(names(term_attr$dataClasses), data)

  structure(
    list(
      model = model,
      data = data,
      formula = original_formula,
      type = "GLM",
      model_specification = list(
        family = make_modfam("binomial", "logistf"),
        regs = list(
          categorical = intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses != "numeric")], term_attr$term.labels),
          metric = intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses == "numeric")], term_attr$term.labels),
          interactions = list_interaction(notation = ":", term_attr = term_attr)
        ),
        intercept = term_attr$intercept == 1
      ),
      inference = "frequentist",
      pseudo_posterior = list(
        args = list(coefs = coef(model), vcov = (model)),
        fun = function(model, ndraws, ...) {
          MASS::mvrnorm(
            n = ndraws, mu = model[["pseudo_posterior"]][["args"]][["coefs"]],
            Sigma = model[["pseudo_posterior"]][["args"]][["vcov"]], ...
          )
        }
      )
    ),
    class = c("CompInt_model", "from logistf")
  )
}

################################################################################

model_transform <- function(model_fit, data) {
  if ("CompInt_model" %in% class(model_fit)) {
    return(model_fit)
  }
  if ("glm" %in% class(model_fit)) {
    return(glm_to_compint(model_fit))
  }
  if ("logistf" %in% class(model_fit)) {
    if (is.null(data)) {
      stop("Model fits of the class 'logistf' do not store the data used to fit them.\n Please specify the variable 'data' in the function call.")
    } else {
      return(logistf_to_compint(model_fit, data))
    }
  } else {
    stop(paste0("The CompInt package does not offer automatic transform of model fits of class '", class(model_fit), "'.\n Please use the 'various_to_compint' function to manually transform this object first."))
  }
}







################################################################################

#' @export
coef.CompInt_model <- function(x, ...) {
  return(x[["pseudo_posterior"]][["args"]][["coefs"]])
}

#' @export
vcov.CompInt_model <- function(x, ...) {
  return(x[["pseudo_posterior"]][["args"]][["vcov"]])
}


################################################################################

#' Draws samples from the parameter distribution of a CompInt_model object.
#'
#' This function generates draws from the parameter distribution of a CompInt_model object.
#'
#' @param object A CompInt_model object.
#' @param ndraws Number of draws to generate.
#' @param seed Seed for the random number generator.
#' @param ... Additional arguments passed to the underlying draw function. This function is stored in model$pseudo_posterior$fun.
#'
#' @return A matrix of draws from the parameter distribution.
#'
#' @details
#' #TOFIX!!!
#'
#' @examples
#' \dontrun{
#' # Example usage
#' glm <- ### some GLM needs to be fit!!! #TOFIX!!!
#'   model <- glm_to_compint(glm)
#' draws <- draws_from_paramdist(model, ndraws = 1000, seed = 123)
#' }
#'
#' # TOFIX: is importFrom needed here? I mean it depends on the pseudo_posterior fun
#'
#' @export

draws_from_paramdist <- function(model, ndraws = 1000, seed = NULL, ...) {
  check_model_class(model, "model")
  if (is.null(seed)) {
    stop("Generating draws from the parameter distribution is a pseudo random process. Please specify a seed.")
  }
  set.seed(seed)
  return(model[["pseudo_posterior"]][["fun"]](model = model, ndraws = ndraws, ...))
}
