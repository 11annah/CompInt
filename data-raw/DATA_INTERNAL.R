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

usethis::use_data(Inverse.Functions, overwrite = TRUE,internal=TRUE)
