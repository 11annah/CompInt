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

data_prep<-function(mod){
  dat<-mod[["data"]][,regs(mod)]
  dat<-dat[complete.cases(dat),]
  return(dat)
}


##
make_dummy_coded_data<-function(mod,dat,separate_interactions=FALSE){
  check_model_class(mod,"mod")
  if(any(!complete.cases(dat))){
    stop("The data provided for empirical integration contains NA values.")
  }
  if(isFALSE(separate_interactions)){
  cat_vars<-mod[["model_specification"]][["regs"]][["categorical"]]
  }else{
    #TOFIX!!
  }
  #return(dcDat)
}


