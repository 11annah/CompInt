import torch
from torch import exp, minimum, maximum, erf, sqrt

from scipy.stats import norm

def make_inv_link_function(option):
  
    if option == "id":
        def inverse_link_function(x):
          return x
        
    if option == "torch_exp":
        def inverse_link_function(x):
          return(torch.exp(x))

    if option == "torch_plogis":
        def inverse_link_function(x):
          result = 1 / (1 + torch.exp(-x))
          return result

    if option == "torch_inv_probit":
        def inverse_link_function(x):
          epsilon = 2.220446049250313e-16
          thresh = -torch.tensor(norm.ppf(epsilon), dtype=torch.double)
          x = torch.minimum(torch.maximum(x, -thresh), thresh)
          result = 0.5 * (1 + torch.erf(x / sqrt(torch.tensor(2.0, dtype=torch.double))))
          return result
      
    return inverse_link_function
