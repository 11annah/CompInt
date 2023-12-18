id<-function(x){x}

torch_plogis<-function(x){1/(1+torch_exp(-x))}

torch_inv_probit<-function (eta) {
  thresh <- -qnorm(.Machine$double.eps)
  eta <- torch_minimum(torch_maximum(eta, -thresh), thresh)
  0.5*(1+torch_erf(eta/sqrt(2)))}

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

