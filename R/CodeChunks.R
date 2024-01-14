run_in_parent <- function(func,pos=1){
  environment(func) <- parent.frame(pos)
  func(envir=environment(func))
}

################################################################################

int_for_RIunif_empirical <- function(envir){
  if(exists("ints.RIunif_empirical")){
  data_rest <- data_prep(mod=model, data = newdata, separate_interactions = separate_interactions)
  data_rest <- data_rest[,-which(names(data_rest)==reg_of_interest),drop=FALSE]
  EmpDat <- make_dummy_coded_data(mod=model, dat=data_rest, reg_of_interest = reg_of_interest, separate_interactions = separate_interactions)
  if(assumption=="A.I") {data <- as.data.frame(do.call(expand.grid,EmpDat))
  }else{
    data <- EmpDat
  }
  ints <- ints.RIunif_empirical
  names(ints) <- reg_of_interest

  assign("data",data,envir=envir)
  assign("ints",ints,envir=envir)
}}
