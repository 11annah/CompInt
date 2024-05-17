run_in_parent <- function(func,pos=1,...){
  environment(func) <- parent.frame(pos)
  func(envir=environment(func),...)
}

################################################################################

getting_situated1 <- function(envir){
  assign("model",model_transform(model_fit,data=data),envir=envir)#This needs to be extended depending on the model structure functions that are written #TOFIX
  reg_naming_check(model)
  if(is.null(integration)){stop("The argument 'integration' must be specified.")}#TOFIX check that it is from the right set of functions!
}

getting_situated2 <- function(envir){
  assumptionstop(assumption)
  assign("ellipsisvars",names(list(...)),envir=envir)
  for (variablename in ellipsisvars) {
    assign(variablename, list(...)[[variablename]], envir = envir)
  }

  if(separate_interactions && !model[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}
}

getting_inverse <- function(envir){
  if(!"inverse_link"%in%ellipsisvars){
    if(!model[["model_specification"]][["family"]][["Link"]] %in% Inverse.Functions[["Link"]]){
      stop("Unfortunately, there is no default inverse link function for the given model's Link='",model[["model_specification"]][["family"]][["Link"]],"'.\n Please specify the variable 'inverse_link' in the function call")
    }else{
      assign("inverse_link",Inverse.Functions[["Inverse"]][which(Inverse.Functions[["Link"]]==model[["model_specification"]][["family"]][["Link"]])],envir=envir)}
  }
}


empirical_Int_catmet_handling <- function(envir){
  assign("regsM",model[["model_specification"]][["regs"]][["metric"]],envir=envir)
  assign("regsC",model[["model_specification"]][["regs"]][["categorical"]],envir=envir)

  assign("Check",if(is.null(newdata)){model[["data"]]}else{newdata},envir=envir)
  assign("is.bin",binary_regs(Check, col=reg_of_interest, catRIbin = catRIbin),envir=envir)
  if(!is.null(is.bin)){
    assign("newdata",is.bin,envir=envir)
    assign("regsM",regsM[-which(regsM==reg_of_interest)],envir=envir)
    assign("regsC",c(regsC,reg_of_interest),envir=envir)
  }

  if(is.null(reg_of_interest)){
    assign("continue_metric", TRUE,envir=envir)
    assign("continue_categorical", FALSE,envir=envir)
  }else{
    assign("continue_metric",reg_of_interest%in%regsM,envir=envir)
    assign("continue_categorical",reg_of_interest%in%regsC,envir=envir)
  }

  if(continue_categorical){assign("RItype", "categorical",envir=envir)
  }else{assign("RItype", "metric",envir=envir)}
}

data_asmpt <- function(envir){
  assign("data_asmpt",data_according_to_assumptions(mod=model,assumption=assumption,newdata=newdata,reg_of_interest=reg_of_interest,RItype=RItype),envir=envir)
  if(length(regsC)==0 || is.null(data_asmpt)){
    assign("EmpDat",data_asmpt,envir=envir)
  }else{
    #if(all(regsC==reg_of_interest) & any(assumption %in% c("A.I","A.II'"))){assign("EmpDat",data_asmpt,envir=envir)
    # }else{
      assign("EmpDat",make_dummy_coded_data(mod=model,dat=data_asmpt,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions),envir=envir)
     #}
    }
}


prepping_for_catRI <- function(envir,i){
  assign("RIvals",RIvals_prep[[i]][["vals"]],envir=envir)
  assign("ref_cat",RIvals_prep[[i]][["ref_cat"]],envir=envir)
  assign("nonref_cats",RIvals_prep[[i]][["nonref_cats"]],envir=envir)
}



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

catch_python_warnings <-  function(){
  if (reticulate::py_run_string("len(warnings._warnings)") > 0) {
    cat("There were Python warnings. Call CompInt_warnings() to view them.\n",fill=TRUE)
  }
}
