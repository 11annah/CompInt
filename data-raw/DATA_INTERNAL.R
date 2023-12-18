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
  })
)



usethis::use_data(Inverse.Functions,ChunkList, overwrite = TRUE,internal=TRUE)
