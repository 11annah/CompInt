torch <- NULL
torchquad <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  torch <<- reticulate::import("torch", delay_load = TRUE)
  torchquad <<- reticulate::import("torchquad", delay_load = TRUE)
  check_py_modules()
}
