from functools import reduce
import operator
from operator import mul
import torch
from torchquad import MonteCarlo, set_up_backend

set_up_backend("torch", data_type="float32")
mc = MonteCarlo()

with open('inst/python_scripts/InverseFunctions.py', 'r') as f:
    script_code = f.read()

exec(script_code)


def integrate_LPmods(ints,data,LinPred,thetas,fun=None,grad_variable=None):
  if fun is not None:
    inv_link_fun = make_inv_link_function(fun)
  else:
    inv_link_fun = None
    
  tensor_dict = {key: torch.tensor(value, dtype=torch.float64).view(-1, 1) for key, value in data.items()}
  globals().update(tensor_dict)    
    
  global theta
  global domains
    
  theta = torch.tensor([0]+thetas, dtype=torch.double)
  domains=list(ints.values())
  
  def function(x):
    x_values = [x[:, i].repeat(len(list(data.values())[0]), 1) for i in range(x.size(1))]
    x_dict = dict(zip(ints.keys(), x_values))
    globals().update(x_dict) 
    
    #So far only Unif #TOFIX
    norm = reduce(mul, (abs(pair[0] - pair[1]) for pair in domains))
    
    if inv_link_fun is not None:
        LinPred_val = norm * eval(f'inv_link_fun({LinPred})')
    else:
        LinPred_val = norm * eval(LinPred)
        
    return(LinPred_val.mean(dim=0))
    
  final_result = mc.integrate(
                              function,
                              dim=len(domains),
                              N=10000,
                              integration_domain=domains,
                              backend="torch",
                              )
  return(final_result)
  
