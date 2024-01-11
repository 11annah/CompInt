id <- function(x) {
  x
}

torch_exp <- function(x) {
  exp(x)
}

torch_plogis <- function(x) {
  1 / (1 + exp(-x))
}

torch_inv_probit <- function(eta) {
  thresh <- -qnorm(.Machine$double.eps)
  eta <- min(max(eta, -thresh), thresh)
  pnorm(eta)
}
