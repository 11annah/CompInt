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
