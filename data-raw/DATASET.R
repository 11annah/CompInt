## code to prepare `DATASET` dataset goes here
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

ChunkList <- list(
  getting_situated = quote({
    assumptionstop(assumption)
    ellipsisvars <- names(list(...))
    for (variablename in ellipsisvars) {
      assign(variablename, list(...)[[variablename]], envir = environment())
    }
    model<-model_transform(model_fit,data=data)
  })
)


usethis::use_data(DATASET, overwrite = TRUE)
