import torch
from torch import exp, minimum, maximum, erf, sqrt

def identity(x):
    return x

def torch_plogis(x):
    return 1 / (1 + exp(-x))

def torch_inv_probit(eta):
    thresh = -norm.ppf(2.220446049250313e-16)  # Equivalent to qnorm(.Machine$double.eps) in R
    eta = minimum(maximum(eta, -thresh), thresh)
    return 0.5 * (1 + erf(eta / sqrt(2)))
