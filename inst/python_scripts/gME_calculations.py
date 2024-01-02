from functools import reduce
import operator
import torch
with open('inst/python_scripts/InverseFunctions.py', 'r') as f:
    script_code = f.read()

exec(script_code)

def create_matrix_function_emp(Mat, vec_list, grad_variable=None):
  
  if grad_variable is None:
     def matrix_function(val_list):
         replacement_dict = {key: value for inner_dict in val_list for key, value in inner_dict.items()}
         replaced_matrix = [[eval(element, replacement_dict) for element in row] for row in Mat]
         
         new_tensor = torch.tensor(replaced_matrix, dtype=torch.double)
    
         return new_tensor
       
  if grad_variable is not None:
     def matrix_function(val_list):
            print("val_list:",val_list)
            index = vec_list.index(grad_variable)
            removed_vec_list = vec_list.pop(index)
            gradv_val = val_list.pop(index)
            
            grad_variable_val = torch.tensor(gradv_val[grad_variable[0]], requires_grad = True)
            
            replacement_dict = {key: value for inner_dict in val_list for key, value in inner_dict.items()}
            replaced_matrix = [
                            [str(cell) if isinstance(cell, (int, float)) else str(replacement_dict.get(cell, cell)) for cell in row]
                            for row in Mat
            ]
            
            evaluated_matrix = [
            [element.replace(grad_variable[0], str("grad_variable_val")) for element in row]
            for row in replaced_matrix
            ]
            
            variables = {'grad_variable_val': grad_variable_val}
            # Evaluate the matrix using PyTorch tensors
            processed = [[eval(element, variables) for element in row] for row in evaluated_matrix]
            
            
            return processed, grad_variable_val
       
  return matrix_function
         



  
def make_result_LinPred_emp(Mat, vec_list,thetas, val_lists, val_list2=None,grad_variable=None,fun=None):
    matfun = create_matrix_function_emp(Mat=Mat, vec_list=vec_list, grad_variable=grad_variable)
    torched_thetas = torch.tensor(thetas, dtype=torch.double)
    
    print("val_lists:",val_lists)
    
    if grad_variable is None:
      if val_list2 is not None:
        Matfun2 = matfun(val_list2)
        prod2 = torch.dot(torched_thetas, torch.prod(Matfun2, dim=1))
        res2 = eval(f'{fun}(prod2)') if fun is not None else prod2
      else:
        res2 = 0
        
      values = []
      
      for val_list in val_lists:
        Matfun1 = matfun(val_list)
        prod1 = torch.dot(torched_thetas, torch.prod(Matfun1, dim=1))
        res1 = eval(f'{fun}(prod1)') if fun is not None else prod1
        values.append(res1-res2)
      
    else:
      values = []
      for val_list in val_lists:
        Matfun, grad_variable_val = matfun(val_list=val_list)
      
        if len(Matfun[0])>1:
          Matfun_prod = [[row[0] * row[1], *row[2:]] for row in Matfun]
      
        Matfun_tensors = [torch.tensor(row[0]) if not isinstance(row[0], torch.Tensor) else row[0] for row in Matfun_prod]
      
        prod = [[row[0] * row[1]] for row in list(zip(torched_thetas,Matfun_tensors))]
      
        stacked_prod = torch.stack([tensor for sublist in prod for tensor in sublist])
        
        sum_prod = stacked_prod.sum()
      
        if fun is not None:
          inv_link_fun = make_inv_link_function(fun)
          res = inv_link_fun(sum_prod)
        else:
          res = sum_prod
        res.backward()

        values.append(grad_variable_val.grad.item())
      
    result = sum(values) / len(values)
    return result
  
  
  
  
  
  




