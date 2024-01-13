## Inverse functions

Inverse.Functions<-data.frame(Link=c("logit",
                                     "logistf",
                                     "log",
                                     "identity",
                                     "probit"),
                              Inverse=c("torch_plogis",
                                        "torch_plogis",
                                        "torch_exp",
                                        "id",
                                        "torch_inv_probit")
)

################################################################################
## Code chunks

ChunkList <- list(
  getting_situated = quote({
    model<-model_transform(model_fit,data=data)#This needs to be extended depending on the model structure functions that are written #TOFIX
    reg_naming_check(model)

    if(is.null(distribution)){stop("The argument 'distribution' must be specified.")}#TOFIX check that it is from the right set of functions!
    distribution(model = model)

    assumptionstop(assumption)
    ellipsisvars <- names(list(...))
    for (variablename in ellipsisvars) {
      assign(variablename, list(...)[[variablename]], envir = environment())
    }

    if(separate_interactions & !model[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}
    }),

  getting_inverse = quote({
    if(!"inverse_link"%in%ellipsisvars){
      if(!model[["model_specification"]][["family"]][["Link"]] %in% Inverse.Functions[["Link"]]){
        stop("Unfortunately, there is no default inverse link function for the given model's Link='",model[["model_specification"]][["family"]][["Link"]],"'.\n Please specify the variable 'inverse_link' in the function call")
      }else{inverse_link<-Inverse.Functions[["Inverse"]][which(Inverse.Functions[["Link"]]==model[["model_specification"]][["family"]][["Link"]])]}
    }
  }),

  empirical_Int_catmet_handling = quote({
    regsM<-model[["model_specification"]][["regs"]][["metric"]]
    regsC<-model[["model_specification"]][["regs"]][["categorical"]]

    Check<-if(is.null(newdata)){model[["data"]]}else{newdata}# potentially #TOFIX when using 'subset'!
    is.bin<-binary_regs(Check, col=reg_of_interest, catRIbin = catRIbin)
    if(!is.null(is.bin)){
      newdata<-is.bin
      regsM<-regsM[-which(regsM==reg_of_interest)]
      regsC<-c(regsC,reg_of_interest)
    }

    if(is.null(reg_of_interest)){
      continue_metric <- TRUE
      continue_categorical <- FALSE
    }else{
      continue_metric<-reg_of_interest%in%regsM
      continue_categorical<-reg_of_interest%in%regsC
    }

    if(continue_categorical){RItype <- "categorical"
    }else{RItype <- "metric"}
  }),

  data_asmpt__plus__coef_draws = quote({
    data_asmpt<-data_according_to_assumptions(mod=model,assumption=assumption,newdata=newdata,reg_of_interest=reg_of_interest,RItype=RItype)
    if(length(regsC)==0 | is.null(data_asmpt)){
      EmpDat<-data_asmpt
    }else{
      if(all(regsC==reg_of_interest) & any(assumption %in% c("A.I","A.II'"))){EmpDat<-data_asmpt}else{
        EmpDat<-make_dummy_coded_data(mod=model,dat=data_asmpt,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)}}

    coef_draws<-draws_from_paramdist(model=model,ndraws=ndraws,seed=seed,...)
  }),

  prepping_for_catRI = quote({
    RIvals_prep<-dealing_with_catRI(dat=EmpDat,RIcat_raw=RIcat_raw,g_theta=eval_g_theta_at_point,RIname=reg_of_interest)
    RIvals<-RIvals_prep[["vals"]]
    ref_cat<-RIvals_prep[["ref_cat"]]
    nonref_cats<-RIvals_prep[["nonref_cats"]]
  })
)



usethis::use_data(Inverse.Functions,ChunkList, overwrite = TRUE,internal=TRUE)
