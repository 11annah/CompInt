from functools import reduce
import operator
import torch
with open('inst/python_scripts/InverseFunctions.py', 'r') as f:
    script_code = f.read()

exec(script_code)


def simplegrad(ints,data,LinPred,thetas,fun=None,grad_variable=None):
  if fun is not None:
    inv_link_fun = make_inv_link_function(fun)
  else:
    inv_link_fun = None
    
  tensor_dict = {key: torch.tensor(value, dtype=torch.float64).view(-1, 1) for key, value in data.items()}
  globals().update(tensor_dict)    
  
  domains=list(ints.values())
  
  def function(x):
    x_dict = dict(zip(ints.keys(), x_values))
    
    
    
  
