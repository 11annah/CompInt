#' #TOFIX
#'
#' #TOFIX
#'
#' @param mod A model object of class "CompInt".
#' @param separate_interactions Logical, indicating whether interaction terms should be denoted and treated as a separate regressor.
#'
#' @return A character string representing the linear predictor.
#'
#' @details
#' #TOFIXThe function retrieves the coefficients from the model object and constructs the linear predictor based on the model formula.
#' #TOFIX!!!
#'
#' @examples
#' \dontrun{
#' # Example usage
#' #TOFIXglm <- ###some GLM needs to be fit!!! #TOFIX!!!
#' #TOFIXmodel <- glm_to_compint(glm)
#' #TOFIXlinear_pred <- produce_model_definition(model, reg_of_interest = "TOFIX", separate_interactions = TRUE)
#' }
#'
#'
#' @importFrom stringr str_replace
#'
#' @export


produce_model_definition <- function(mod,separate_interactions=FALSE,name_targetvar=NULL){
  #Insert stop message
  LinPred <- make_linear_predictor(mod=mod,reg_of_interest=NULL,separate_interactions=separate_interactions)
  Y <- ifelse(is.null(name_targetvar),"Y",name_targetvar)
  #TOFIX
  indexing = "[l]"
  model_formula <- paste0(
    "E[",Y,"|(",paste(unlist(LinPred$reg_groups),collapse=", "),")] = ",gsub(gsub("([][\\^$.|?*+()])", "\\\\\\1", indexing, perl=TRUE),"",LinPred$non_vectorized)
  )
  output_plain <- list(model_formula=model_formula)
  #EXTEND
  return(output_plain)
}

# Documentation is missing!! #TOFIX !
#' @export
make_g_theta<-function(model_type,linear_predictor=NULL,inverse_link=NULL,vectorized=TRUE,...){
  # To be expanded (ideally)
  if(model_type %in% c("GLM","GLMM")){
    if(is.null(linear_predictor)){stop("Models of type 'GLM' and 'GLMM' require a linear predictor.")}
    if(isFALSE(vectorized)){
    if(is.null(inverse_link)){
      g_theta<-linear_predictor$non_vectorized
      warning("No inverse link function was specified, which is generally not intended. Returning linear predictor.")
    }else{g_theta<-paste0(inverse_link,"(",linear_predictor$non_vectorized,")")}
  }else{
    if(is.null(inverse_link)){
      g_theta<-paste0("sapply(l,function(l)",linear_predictor$vectorized,")")
      warning("No inverse link function was specified, which is generally not intended. Returning linear predictor.")
    }else{g_theta<-paste0(inverse_link,"(",paste0("sapply(l,function(l)",linear_predictor$vectorized,")"),")")}
    }
  }
  return(g_theta)
}



# Documentation is missing!! #TOFIX !
#' @export
integrate_wrt_pm<-function(fun){
  py_fun <- reticulate::r_to_py(fun)
  #tochquad <- reticulate::import("torchquad")
  reticulate::source_python("inst/python_scripts/test.py")
  add(5,10)
  #return(reticulate::py_to_r(output))
}






