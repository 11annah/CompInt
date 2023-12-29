import torch

def replace_vars(expr,vec_list,val_list,nested=False):
    if nested is False:
      for var, val in zip(vec_list, val_list):
          expr = expr.replace(var, f'{val}')
          
    if nested is True:
      for i in range(0, len(vec_list)):
        for var, val in zip(vec_list[i], val_list[i]):
            expr = expr.replace(var, f'{val}')
        
    return expr
  

def create_matrix_function(Mat, vec_list, grad_variable=None):
  if grad_variable is None:
     def matrix_function(val_list):
         replaced_matrix = [[replace_vars(expr, vec_list, val_list, nested=True) for expr in row] for row in Mat]
    
         processed = [[eval(element) for element in row] for row in replaced_matrix]
    
         new_tensor = torch.tensor(processed, dtype=torch.double)
    
         return new_tensor
       
  if grad_variable is not None:
     def matrix_function(gradv_val,val_list):
            grad_variable_val = torch.tensor(gradv_val, requires_grad = True)
            
            index = vec_list.index(grad_variable)
            removed_vec_list = vec_list.pop(index)
            
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
         



  
def make_result_LinPred(Mat, vec_list, val_list,thetas,grad_variable=None,gradv_val=None,fun=None):
  
    matfun = create_matrix_function(Mat=Mat, vec_list=vec_list, grad_variable=grad_variable)
    torched_thetas = torch.tensor(thetas, dtype=torch.double)
    
    if grad_variable is None:
      Matfun = matfun(val_list)
      prod = torch.dot(torched_thetas, torch.prod(Matfun, dim=1))
      result = prod
      
    else:
      Matfun, grad_variable_val = matfun(gradv_val=gradv_val,val_list=val_list)
      
      if len(Matfun[0])>1:
        Matfun_prod = [[row[0] * row[1], *row[2:]] for row in Matfun]
      
      Matfun_tensors = [torch.tensor(row) if not isinstance(row[0], torch.Tensor) else row[0] for row in Matfun_prod]
      
      prod = [[row[0] * row[1]] for row in list(zip(torched_thetas,Matfun_tensors))]
      
      flat_prod = [tensor for sublist in prod for tensor in sublist]

      stacked_prod = torch.stack(flat_prod)

      sum_prod = stacked_prod.sum()
      
      if fun is not None:
        res = eval(f'{fun}(grad_variable_val)')
      else:
        res = sum_prod

      res.backward()

      result = grad_variable_val.grad
      
      
    return result.item()
 
