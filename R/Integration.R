#' Extracts the linear predictor from a model formula.
#'
#' This function extracts the linear predictor from a model formula and formats it as a string.
#'
#' @param mod A model object of class "CompInt".
#' @param reg_of_interest The regressor of interest. If specified, the original regressor name in the linear predictor will be replaced with "RI".
#' @param separate_interactions Logical, indicating whether interaction terms should be denoted and treated as a separate regressor.
#'
#' @return A character string representing the linear predictor.
#'
#' @details
#' The function retrieves the coefficients from the model object and constructs the linear predictor based on the model formula.
#' #TOFIX!!!
#'
#' @examples
#' \dontrun{
#' # Example usage
#' glm <- ###some GLM needs to be fit!!! #TOFIX!!!
#' model <- glm_to_compint(glm)
#' linear_pred <- make_linear_predictor(model, reg_of_interest = "TOFIX", separate_interactions = TRUE)
#' }
#'
#'
#' @importFrom stringr str_replace
#'
#' @export


make_linear_predictor<-function(mod,reg_of_interest=NULL,separate_interactions=FALSE){
  check_model_class(mod,"mod")

  coefs<-coef(mod)
  if(is.null(names(coefs))){stop("The 'coefs' entry of the CompInt model-object is unnamed.")}

  model_coefficients<-paste0("theta[",1:length(coefs),"]")

  if(!is.null(reg_of_interest)){
    if(separate_interactions){
      names(coefs)<-sub(paste0("^",reg_of_interest),"RI",names(coefs))
    }
    if(!separate_interactions){
      if(!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}
      names(coefs)<-sub(reg_of_interest,"RI",names(coefs))
    }}

  if(mod[["model_specification"]][["intercept"]]){
    if(!grepl("tercept",names(coefs)[1])){warning("The first coefficient's name does not contain the term intercept, but is being used as intercept anyways.")}
    model_terms <- model_coefficients[1]
    start<-2
  }else{model_terms <- 0
  start<-1}

  for (i in c(start:length(coefs))) {
    model_terms <- c(model_terms, paste0("+ ", model_coefficients[i], " * ", paste0(names(coefs))[i],"[l]"))
  }

  if(separate_interactions){
    linear_predictor<-paste(stringr::str_replace(model_terms,mod[["model_specification"]][["regs"]][["interactions"]][["notation"]],"_x_"),collapse=' ')
  }else{
    linear_predictor<-paste(stringr::str_replace(model_terms,mod[["model_specification"]][["regs"]][["interactions"]][["notation"]],"[l]*"),collapse=' ')
  }

  return(linear_predictor)
}

make_g_theta<-function(model_type,linear_predictor=NULL,...){
  # To be expanded!
  if(model_type %in% c("GLM","GLMM")){
    if(is.null(linear_predictor)){stop("Models of type 'GLM' and 'GLMM' require a linear predictor.")}
    if(is.null(inverse_link)){
      g_theta<-linear_predictor
    }else{
      g_theta<-paste0(inverse_link,"(",linear_predictor,")")}
  }
}


#data_according_to_assumptions
    # this is for empirical integration

## matching formula expressions to categories


