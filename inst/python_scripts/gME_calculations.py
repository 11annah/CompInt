from functools import reduce
import operator
import torch
with open('inst/python_scripts/InverseFunctions.py', 'r') as f:
    script_code = f.read()

exec(script_code)

def replace_vars(expr,vec_list,val_list,nested=False):
    if nested is False:
      for var, val in zip(vec_list, val_list):
          expr = expr.replace(var, f'{val}')
          
    if nested is True:
      for i in range(0, len(vec_list)):
        for var, val in zip(vec_list[i], val_list[i]):
            expr = expr.replace(var, f'{val}')
        
    return expr
  

def create_matrix_function_emp(Mat, vec_list, grad_variable=None):
  if grad_variable is None:
     def matrix_function(val_list):
         replaced_matrix = [[replace_vars(expr, vec_list, val_list, nested=True) for expr in row] for row in Mat]
    
         processed = [[eval(element) for element in row] for row in replaced_matrix]
    
         new_tensor = torch.tensor(processed, dtype=torch.double)
    
         return new_tensor
       
  if grad_variable is not None:
     def matrix_function(val_list):
            index = vec_list.index(grad_variable)
            removed_vec_list = vec_list.pop(index)
            gradv_val = val_list.pop(index)
          
            grad_variable_val = torch.tensor(gradv_val, requires_grad = True)
            
            replaced_matrix = [[replace_vars(expr, vec_list, val_list, nested=True) for expr in row] for row in Mat]
            
            evaluated_matrix = [
            [element.replace(grad_variable[0], str("grad_variable_val")) for element in row]
            for row in replaced_matrix
            ]
            
            variables = {'grad_variable_val': grad_variable_val}
            # Evaluate the matrix using PyTorch tensors
            processed = [[eval(element, variables) for element in row] for row in evaluated_matrix]
            
            
            return processed, grad_variable_val
       
  return matrix_function
         



  
def make_result_LinPred(Mat, vec_list,thetas, val_list, val_list2=None,grad_variable=None,fun=None):
    matfun = create_matrix_function_emp(Mat=Mat, vec_list=vec_list, grad_variable=grad_variable)
    torched_thetas = torch.tensor(thetas, dtype=torch.double)
    
    if grad_variable is None:
      Matfun1 = matfun(val_list)
      Matfun2 = matfun(val_list2)
      prod1 = torch.dot(torched_thetas, torch.prod(Matfun1, dim=1))
      if grad_variable is None
        prod2 = torch.dot(torched_thetas, torch.prod(Matfun2, dim=1))
      if fun is not None:
        res1 = eval(f'{fun}(prod1)')
        if grad_variable is None
           res2 = eval(f'{fun}(prod2)')
      else:
        res1 = prod1
        if grad_variable is None
           res2 = prod2
        
      if grad_variable is None
        result = res1-res2
      else
        result = res1
      
    else:
      Matfun, grad_variable_val = matfun(val_list=val_list)
      
      if len(Matfun[0])>1:
        Matfun_prod = [[reduce(operator.mul, row)] for row in Matfun]
      
      Matfun_tensors = [torch.tensor(row) if not isinstance(row[0], torch.Tensor) else row[0] for row in Matfun_prod]
      
      prod = [[row[0] * row[1]] for row in list(zip(torched_thetas,Matfun_tensors))]
      
      flat_prod = [tensor for sublist in prod for tensor in sublist]

      stacked_prod = torch.stack(flat_prod)

      sum_prod = stacked_prod.sum()
      
      if fun is not None:
        inversef = make_inv_link_function(fun)
        res = inversef(sum_prod)
      else:
        res = sum_prod

      res.backward()

      result = grad_variable_val.grad
      
      
    return result.item()
  
  
  
  
  
  
  import torch



#### Try something like this please:::::::::
#### Maybe even with all thetas at once
def make_result_LinPred_at_once(Mat, vec_list, thetas, val_lists, grad_variable=None, fun=None):
    matfun = create_matrix_function_emp(Mat=Mat, vec_list=vec_list, grad_variable=grad_variable)
    torched_thetas = torch.tensor(thetas, dtype=torch.double)

    if grad_variable is None:
        Matfun = torch.stack([matfun(val_list) for val_list in val_lists])
        prod = torch.matmul(torched_thetas, torch.prod(Matfun, dim=2).t())
        result = eval(f'{fun}(prod)') if fun else prod
    else:
        results = []
        grad_variable_vals = []

        for val_list in val_lists:
            Matfun, grad_variable_val = matfun(val_list=val_list)
            Matfun_prod = Matfun.prod(dim=2)
            prod = (torched_thetas * Matfun_prod).sum(dim=1)
            result = eval(f'{fun}(prod)') if fun else prod

            results.append(result)
            grad_variable_vals.append(grad_variable_val)

        result = torch.stack(results)
        grad_variable_val = torch.stack(grad_variable_vals)
        ##I still need the arithmetic mean

    return result

