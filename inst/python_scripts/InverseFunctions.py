import torch
from torch import exp, minimum, maximum, erf, sqrt

from scipy.stats import norm

def make_inv_link_function(option):
    x=torch.tensor(x,dtype=torch.double)
  
    if option == "id":
        def inverse_link_function(x):
          x=torch.tensor(x,dtype=torch.double)
          return x

    if option == "torch_plogis":
        def inverse_link_function(x):
          x=torch.tensor(x,dtype=torch.double)
          result = 1 / (1 + exp(-x))
          return result

    if option == "torch_inv_probit":
        def inverse_link_function(x):
          x=torch.tensor(x,dtype=torch.double)
          epsilon = 2.220446049250313e-16
          thresh = -torch.tensor(norm.ppf(epsilon), dtype=torch.double)
          x = torch.minimum(torch.maximum(x, -thresh), thresh)
          result = 0.5 * (1 + torch.erf(x / sqrt(torch.tensor(2.0, dtype=torch.double))))
          return result
      
    return inverse_link_function
