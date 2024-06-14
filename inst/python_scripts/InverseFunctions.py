from typing import Callable, Optional

import torch
from scipy.stats import norm


def make_inv_link_function(option: str) -> Optional[Callable[[torch.Tensor], torch.Tensor]]:
    def identity(x: torch.Tensor) -> torch.Tensor:
        return x

    def torch_exp(x: torch.Tensor) -> torch.Tensor:
        return torch.exp(x)

    def torch_plogis(x: torch.Tensor) -> torch.Tensor:
        return 1 / (1 + torch.exp(-x))

    def torch_inv_probit(x: torch.Tensor) -> torch.Tensor:
        epsilon = 2.220446049250313e-16
        thresh = -torch.tensor(norm.ppf(epsilon), dtype=torch.double)
        x = torch.minimum(torch.maximum(x, -thresh), thresh)
        return 0.5 * (
            1 + torch.erf(x / torch.sqrt(torch.tensor(2.0, dtype=torch.double)))
        )

    options = {
        "id": identity,
        "torch_exp": torch_exp,
        "torch_plogis": torch_plogis,
        "torch_inv_probit": torch_inv_probit,
    }
    return options.get(option)
