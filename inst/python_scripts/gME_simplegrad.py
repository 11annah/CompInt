from typing import Optional

import torch

from InverseFunctions import make_inv_link_function


def simplegrad(
    data: dict[str, list],
    LinPred: str,
    thetas: list[float],
    grad_variable: str,
    fun: Optional[str] = None,
):
    inv_link_fun = make_inv_link_function(fun) if fun is not None else None

    tensor_dict = {
        key: torch.tensor(value, dtype=torch.float64, requires_grad=True)
        for key, value in data.items()
    }

    globals().update(tensor_dict)

    def grad_for_theta(theta: torch.Tensor) -> float:
        if inv_link_fun is not None:
            LinPred_val = eval(f"inv_link_fun({LinPred})")
        else:
            LinPred_val = eval(LinPred)

        LinPred_val.backward(torch.ones_like(LinPred_val), retain_graph=True)

        gradient = eval(f"{grad_variable}.grad")
        result = torch.mean(gradient)

        return result.item()

    theta_tensor = torch.tensor(thetas, dtype=torch.double)
    final_res = grad_for_theta(theta_tensor)
    return final_res
