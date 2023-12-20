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
    assumptionstop(assumption)
    ellipsisvars <- names(list(...))
    for (variablename in ellipsisvars) {
      assign(variablename, list(...)[[variablename]], envir = environment())
    }
    model<-model_transform(model_fit,data=data)#This needs to be extended depending on the model structure functions that are written #TOFIX
    reg_naming_check(model)
    newdata <- newdata_subset_merge(newdata,subset,mod=model)
    }),

  getting_inverse = quote({
    if(!"inverse_link"%in%ellipsisvars){
      if(!model[["model_specification"]][["family"]][["Link"]] %in% Inverse.Functions[["Link"]]){
        stop("Unfortunately, there is no default inverse link function for the given model's Link='",model[["model_specification"]][["family"]][["Link"]],"'.\n Please specify the variable 'inverse_link' in the function call")
      }else{inverse_link<-Inverse.Functions[["Inverse"]][which(Inverse.Functions[["Link"]]==model[["model_specification"]][["family"]][["Link"]])]}
    }
  })
)



usethis::use_data(Inverse.Functions,ChunkList, overwrite = TRUE,internal=TRUE)
