torch <- NULL
torchquad <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::use_virtualenv("r-CompInt", required = FALSE)
  # use superassignment to update global reference to scipy
  torch <<- reticulate::import("torch", delay_load = TRUE)
  torchquad <<- reticulate::import("torchquad", delay_load = TRUE)

  invisible(reticulate::py_config())
  if (!reticulate::py_available()) {
    cat("Calling reticulate::py_available() returns 'FALSE'. Unless this is remidied and the required python modules are installed, you will not be able to access the full functionality of the CompInt package.", fill = TRUE)
  }

  check_py_modules()
}
