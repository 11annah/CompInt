check_model_class<-function(model,inputname){
  message<-paste0("Input ",inputname," is not of the expected class (CompInt_model).")
  if(!inherits(model, "CompInt_model")){
    stop(message)
  }
}

regs <- function(object) {
  check_model_class(object,"object")
  return(unlist(object$model_specification$regs[c("metric","categorical")]))
}

data_prep<-function(mod){
  dat<-mod[["data"]][,regs(mod)]
  dat<-dat[complete.cases(dat),]
}




