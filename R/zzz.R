#ret_torch <- NULL
#ret_torchquad <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::use_virtualenv("r-CompInt", required = FALSE)
  # use superassignment to update global reference to scipy
  #ret_torch <<- reticulate::import("torch",as = "ret_torch", delay_load = TRUE)
  #ret_torchquad <<- reticulate::import("torchquad", as = "ret_torchquad", delay_load = TRUE)

  #invisible(reticulate::py_config())
  #if (!reticulate::py_available()) {
  #  cat("Calling reticulate::py_available() returns 'FALSE'. Unless this is remidied and the required python modules are installed, you will not be able to access the full functionality of the CompInt package.", fill = TRUE)
  #}

  check_py_modules()
}
