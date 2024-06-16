#' @export
check_py_modules <- function(pythonhome = reticulate::py_config()$pythonhome) {
  message("Checking for the required python modules...")
  required_modules <- c("torch", "torchquad")
  missing_modules <- required_modules[!required_modules %in% reticulate::py_list_packages()$package]
  if (length(missing_modules) != 0) {
    cat(sprintf(
      "The python module(s) %s are not installed in the specified pythonhome %s\n",
      toString(missing_modules), pythonhome
    ))
  } else {
    cat("All required python modules are present!\n")
  }
}

#' @export
py_install_CompInt <- function(...,
                               envname = ifelse(reticulate::virtualenv_exists("r-CompInt"), "r-CompInt", "r-reticulate"),
                               new_env = identical(envname, "r-CompInt")) {
  if (new_env && reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_remove(envname)
  }
  reticulate::py_install(packages = c("torch", "torchquad"), envname = envname, ...)
}
