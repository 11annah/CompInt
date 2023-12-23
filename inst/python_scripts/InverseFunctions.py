import torch
from torch import exp, minimum, maximum, erf, sqrt

from scipy.stats import norm

def link_function(x,option):
    x=torch.tensor(x,dtype=torch.double)
  
    if option == "id":
        return x.numpy()

    if option == "torch_plogis":
        result = 1 / (1 + exp(-x))
        return result.numpy()

    if option == "torch_inv_probit":
        epsilon = 2.220446049250313e-16
        thresh = -torch.tensor(norm.ppf(epsilon), dtype=torch.double)
        x = torch.minimum(torch.maximum(x, -thresh), thresh)
        result = 0.5 * (1 + torch.erf(x / sqrt(torch.tensor(2.0, dtype=torch.double))))
        return result.numpy()
