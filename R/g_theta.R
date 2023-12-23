#' Extracts the linear predictor from a model formula.
#'
#' This function extracts the linear predictor from a model formula and formats it as a string.
#'
#' @param mod A model object of class "CompInt".
#' @param reg_of_interest The regressor of interest. Only relevant when separate_interactions=TRUE. #TOFIX
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

  if(mod[["model_specification"]][["intercept"]]){
    if(!grepl("tercept",names(coefs)[1])){warning("The first coefficient's name does not contain the term intercept, but is being used as intercept anyways.")}
    model_terms <- list(list(model_term=model_coefficients[1]))
    start<-2
  }else{model_terms <- 0
  start<-1}

  for (i in start:length(coefs)) {
    if(start==2){j=i}else{j=i+1}
    INT_notation<-paste0("\\",mod[["model_specification"]][["regs"]][["interactions"]][["notation"]])

    REG<-names(coefs)[i]
    TERM<-paste0(model_coefficients[i], " * ", REG,"[l]")
    if(separate_interactions & !is.null(reg_of_interest)){
      if(grepl(reg_of_interest,TERM)){TERM<-stringr::str_replace(TERM,INT_notation,"_x_")}
    }else{TERM<-stringr::str_replace(TERM,INT_notation,"[l] * ")}
    model_terms[[j]] <- list(model_term = TERM,
                             regressors = unlist(strsplit(REG,INT_notation)),
                             coefficient = model_coefficients[i],
                             categorical_element = unlist(sapply(unlist(strsplit(REG,INT_notation)),function(x)wich_reg_is_involved("categorical",mod,x))),
                             metric_element = unlist(sapply(unlist(strsplit(REG,INT_notation)),function(x)wich_reg_is_involved("metric",mod,x)))
    )}

  linpred_novec <- merge_linpred_terms(model_terms)

  #The following would be a good check, but we have to check that it does not clash with make_binary
  #if(!identical(sort(unique(unlist(listels_by_name(model_terms, "categorical_element")))), sort(mod[["model_specification"]][["regs"]][["categorical"]])))){
  #stop("")}

  #if(!length(mod[["model_specification"]][["regs"]][["categorical"]])==0){
  #catTERMS<-listels_by_name(model_terms,"categorical_element")
  #index<-which(lengths(catTERMS)>0)
  #catTERMS<-catTERMS[index]

  #vec_groups<-merge_vectors(create_CatInt_groups(catTERMS))
  #}

  test_model_terms<<-model_terms


  SepEls<-unlist(lapply(listels_by_name(test_model_terms,"model_term"),function(x)strsplit(x,"\\*")), recursive = FALSE)
  theta_vec<-vec_to_c(lapply(SepEls, function(x) trimwd(x[[1]])))
  non_thetas<-lapply(SepEls, function(x) x[-1])
  non_thetas[which(lengths(non_thetas)==0)]<-"1"

  test_non_thetas<<-non_thetas
  test_theta_theta_vec<<-theta_vec


  linear_predictor <- list(vectorized="TBD",#merge_linpred_terms(linpred_terms),
    non_vectorized=linpred_novec)


  return(linear_predictor)
}


# Documentation is missing!! #TOFIX !
#' @export
make_g_theta<-function(model_type,linear_predictor=NULL,inverse_link=NULL,...){
  # To be expanded (ideally)
  if(model_type %in% c("GLM","GLMM")){
    if(is.null(linear_predictor)){stop("Models of type 'GLM' and 'GLMM' require a linear predictor.")}
    if(is.null(inverse_link)){
      g_theta<-linear_predictor
      warning("No inverse link function was specified, which is generally not intended. Returning linear predictor.")
    }else{
      g_theta<-paste0(inverse_link,"(",linear_predictor,")")}
  }
  return(g_theta)
}



# Documentation is missing!! #TOFIX !
#' @export
integrate_wrt_pm<-function(fun){
  py_fun <- reticulate::r_to_py(fun)
  tochquad <- reticulate::import("torchquad")
  reticulate::source_python("inst/python_scripts/test.py")
  add(5,10)
  #return(reticulate::py_to_r(output))
}






