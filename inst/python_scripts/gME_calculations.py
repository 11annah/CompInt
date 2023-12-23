import torch

def matrix_multiply_with_gradient(vector, matrix):
    input_tensor = torch.tensor(vector, dtype=torch.double, requires_grad=True)
    matrix_tensor = torch.tensor(matrix, dtype=torch.double)
    
    # Perform matrix multiplication
    result = torch.matmul(input_tensor,matrix_tensor)
    
    # Backward pass to compute the gradient
    result.backward()
        
    #Access the gradient of the input tensor
    gradient = input_tensor.grad.numpy()
        
    # Clear the gradient for future computations
    input_tensor.grad.zero_()
        
    return gradient


def linear_predictor_torch(vector,matrix,fun,option_argument):
    def inner_function(x):
        return fun(x,option=option_argument)
      
    vec = torch.tensor(vector, dtype=torch.double)#, requires_grad=True)
    mat = torch.tensor(matrix, dtype=torch.double)
    x = torch.matmul(vec, torch.prod(mat, dim=1))
    result = inner_function(x)
    return result


####CONTINUE
### sympy !!!!

