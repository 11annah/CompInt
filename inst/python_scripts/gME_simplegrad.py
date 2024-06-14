from typing import Optional

import torch
from InverseFunctions import make_inv_link_function


def simplegrad(data, LinPred: str, thetas, grad_variable: str, fun: Optional[str]=None):
    if fun is not None:
        inv_link_fun = make_inv_link_function(fun)
    else:
        inv_link_fun = None

    tensor_dict = {
        key: torch.tensor(value, dtype=torch.float64, requires_grad=True)
        for key, value in data.items()
    }
    globals().update(tensor_dict)

    def grad_for_theta(theta):
        if inv_link_fun is not None:
            LinPred_val = eval(f"inv_link_fun({LinPred})")
        else:
            LinPred_val = eval(LinPred)

        LinPred_val.backward(torch.ones_like(LinPred_val), retain_graph=True)

        gradient = eval(f"{grad_variable}.grad")

        result = torch.mean(gradient)

        return result.item()

    # final_res = [grad_for_theta(theta=torch.tensor(entry, dtype=torch.double),LinPred=LinPred,grad_variable=grad_variable,inv_link_fun=inv_link_fun) for entry in thetas]

    final_res = grad_for_theta(theta=torch.tensor(thetas, dtype=torch.double))

    return final_res


def testing(coef_draws):
    print(torch.tensor(coef_draws, torch.double))
