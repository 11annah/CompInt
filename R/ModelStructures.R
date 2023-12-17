#' Functions for transforming specific model objects to CompInt model objects
#'
#' ... Complete documentation !!!
#' @name Changing-model-structures

#' @describeIn Changing-model-structures Transforms a standard glm model into a CompInt model
#' @export

glm_to_compint <- function(model) {
  term_attr<-attributes(terms(model))
  original_formula <- get_original_formula(model)

  structure(list(
    model = model,
    data = model$data,
    formula = original_formula,
    type = "GLM",
    model_specification = list(
      family = family(model),
      regs = list(
        categorical=intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses!="numeric")],term_attr$term.labels),
        metric=intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses=="numeric")],term_attr$term.labels),
        interactions=list_interaction(notation=":",term_attr=term_attr)
      ),
      intercept=term_attr$intercept==1
      ),
    inference = "frequentist",
    pseudo_posterior = list(
      args = list(coefs=coef(model),vcov=vcov(model)),
      fun = function(object,ndraws,...){
        MASS::mvrnorm(n = ndraws, mu=object[["pseudo_posterior"]][["args"]][["coefs"]],
                      object[["pseudo_posterior"]][["args"]][["vcov"]],...)
      }
    )
  ),
  class = c("CompInt_model","from glm")
  )

}


#' @describeIn Changing-model-structures Transforms a logistf model into a CompInt model
#' @export

logistf_to_compint <- function(model, data) {
  original_formula <- get_original_formula(model)
  term_attr<-attributes(terms(model))

  structure(list(
    model = model,
    data = data,
    formula = original_formula,
    type = "GLM",
    model_specification = list(
      family = list("binomial","logistf"),
      regs = list(
                categorical=intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses!="numeric")],term_attr$term.labels),
                metric=intersect(names(term_attr$dataClasses)[which(term_attr$dataClasses=="numeric")],term_attr$term.labels),
                interactions=list_interaction(notation=":",term_attr=term_attr)
    ),
    intercept=term_attr$intercept==1
    ),
    inference = "frequentist",
    pseudo_posterior = list(
                          args = list(coefs=coef(model),vcov=(model)),
                          fun = function(object,ndraws,...){
                          MASS::mvrnorm(n = ndraws, mu=object[["pseudo_posterior"]][["args"]][["coefs"]],
                                        object[["pseudo_posterior"]][["args"]][["vcov"]],...)
                                                       }
     )
  ),
  class = c("CompInt_model","from logistf")
  )

}












################################################################################


regs <- function(object) {
  if(inherits(object, "CompInt_model")) {
    return(unlist(object$model_specification$regs))
  } else {
    stop("Input object is not of the expected class (CompInt_model).")
  }
}



coef.CompInt_model <- function(x, ...) {
    return(x[["pseudo_posterior"]][["args"]][["coefs"]])
}


vcov.CompInt_model <- function(x, ...) {
  return(x[["pseudo_posterior"]][["args"]][["vcov"]])
}







