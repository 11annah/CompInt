### making g_theta
    ## matching formula expressions to categories
          # Here an error message in case someone wrote something like factor(variable) in formula - transformation has to be performed beforehand

make_linear_predictor<-function(mod,reg_of_interest=NULL,separate_interactions=FALSE){
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
  }else{model_terms <- 0}

  for (i in c(2:length(coefs))) {
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

if(model_type %in% c("GLM","GLMM")){
 if(is.null(linear_predictor)){stop("Models of type 'GLM' and 'GLMM' require a linear predictor.")}
if(is.null(inverse_link)){
  g_theta<-linear_predictor
}else{
  g_theta<-paste0(inverse_link,"(",linear_predictor,")")}
}
}

# Here an error message in case someone wrote something like factor(variable) in formula - transformation has to be performed beforehand

data_prep<-function(mod){
  dat<-mod$data[,regs(mod)]
  dat<-dat[complete.cases(dat),]
}
