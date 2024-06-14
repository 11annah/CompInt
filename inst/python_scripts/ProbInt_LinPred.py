from functools import reduce
from operator import mul

import torch
from InverseFunctions import make_inv_link_function
from torchquad import MonteCarlo, set_up_backend

set_up_backend("torch", data_type="float32")
mc = MonteCarlo()

def integrate_LPmods(ints, LinPred, thetas, data=None, fun=None, grad_variable=None):
    if fun is not None:
        inv_link_fun = make_inv_link_function(fun)
    else:
        inv_link_fun = None

    if data is not None:
        tensor_dict = {
            key: torch.tensor(value, dtype=torch.float64).view(-1, 1)
            for key, value in data.items()
        }
        globals().update(tensor_dict)

    global theta
    global domains

    theta = torch.tensor(thetas, dtype=torch.double)
    domains = list(ints.values())

    def function(x):
        if grad_variable is None:
            x_values = [
                x[:, i].repeat(len(list(data.values())[0]), 1) for i in range(x.size(1))
            ]
        else:
            x_values = [
                torch.tensor(
                    x[:, i].repeat(len(list(data.values())[0]), 1),
                    dtype=torch.float64,
                    requires_grad=True,
                )
                for i in range(x.size(1))
            ]

        x_dict = dict(zip(ints.keys(), x_values))
        globals().update(x_dict)

        # So far only Unif #TOFIX
        norm = reduce(mul, (abs(pair[0] - pair[1]) for pair in domains))

        if inv_link_fun is not None:
            LinPred_val = (norm ** (-1)) * eval(f"inv_link_fun({LinPred})")
        else:
            LinPred_val = norm * eval(LinPred)

        if grad_variable is None:
            result = LinPred_val.mean(dim=0)
        else:
            # LinPred_val.backward(torch.ones_like(LinPred_val), retain_graph=True)
            # Issue: needs to be VECTORIZED!!!!
            # LinPred_val[:,0] ....

            LinPred_val.backward(torch.ones_like(LinPred_val), retain_graph=True)

            gradient = eval(f"{grad_variable}.grad")
            result = gradient.mean(dim=0)

        return result

    final_result = mc.integrate(
        function,
        dim=len(domains),
        N=10000,
        integration_domain=domains,
        backend="torch",
    )
    return final_result.item()
