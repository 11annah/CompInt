check_model_class<-function(model,inputname){
  message<-paste0("Input ",inputname," is not of the expected class (CompInt_model).")
  if(!inherits(model, "CompInt_model")){
    stop(message)
  }
}

regs <- function(object) {
  check_model_class(object,"object")
  return(unlist(object[["model_specification"]][["regs"]][c("metric","categorical")]))
}

data_prep<-function(mod,data=NULL,separate_interactions=FALSE){
  if(is.null(data)){data<-mod[["data"]]}
  dat <- data[,regs(mod), drop = FALSE]
  dat <- dat[complete.cases(dat),,drop = FALSE]

  if(separate_interactions){
    if(!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}

    terms<-mod[["model_specification"]][["regs"]][["interactions"]][["terms"]]
    notation<-mod[["model_specification"]][["regs"]][["interactions"]][["notation"]]

    INTs<-data.frame(terms=stringr::str_replace(terms,paste0("\\",notation),"_x_"),
                     expr=stringr::str_replace(terms,paste0("\\",notation),"*"))

    with(dat, {
    for(i in seq_along(INTs$terms)){dat[[INTs$terms[i]]]<-eval(parse(text=INTs$expr[i]))}
    })
  }
  return(dat)
}


RI_and_INT_renaming<-function(mod,names,reg_of_interest,separate_interactions){
  check_model_class(mod,"mod")
  if(!is.null(reg_of_interest)){
    if(separate_interactions){
      if(!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}
      nonINTs<-which(!grepl(paste0("\\",mod[["model_specification"]][["regs"]][["interactions"]][["notation"]]),names))
      names[nonINTs]<-sub(paste0("^",reg_of_interest),"RI",names[nonINTs])
    }
    if(!separate_interactions){
      names<-sub(reg_of_interest,"RI",names)
    }}
  return(names)
}


##
make_dummy_coded_data<-function(mod,dat,reg_of_interest=NULL,separate_interactions=FALSE){
  check_model_class(mod,"mod")
  if(any(!complete.cases(dat))){
    stop("The data provided for empirical integration contains NA values.")
  }
  if(isFALSE(separate_interactions)){
  cat_vars<-mod[["model_specification"]][["regs"]][["categorical"]]
  cat_vars<-RI_and_INT_renaming(mod,cat_vars,reg_of_interest,separate_interactions)
  names(dat)<-RI_and_INT_renaming(mod,names(dat),reg_of_interest,separate_interactions)
  }else{
    #TOFIX!!
  }
  DCdat<-dat
  for(i in seq_along(cat_vars)){
    if (!cat_vars[i] %in% names(DCdat)) {
      stop(paste0("Variable '", cat_vars[i], "' not found in the data frame of observations."))
    }
    DCdat <- cbind(DCdat, model.matrix(~0 + as.factor(DCdat[[cat_vars[i]]])))
    DCdat <- DCdat[, !names(DCdat) %in% cat_vars[i], drop = FALSE]
    names(DCdat)<-sub("as.factor\\(DCdat\\[\\[cat_vars\\[i\\]\\]\\])",cat_vars[i],names(DCdat))
  }
  return(DCdat)
}


