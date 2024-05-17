#' @export
#'
check_py_modules <- function(pythonhome=reticulate::py_config()$pythonhome){
message("Checking for the required python modules...")
required_modules <- c("torch","torchquad")
missing_modules <- required_modules[which(!required_modules %in% reticulate::py_list_packages()$package)]
if(length(missing_modules) != 0){
  cat(paste0("The python module(s) ",paste(missing_modules,collapse=" "),"
                 are not installed in the specified pythonhome",pythonhome,
               ".\n "),fill=TRUE)
}else{
    cat("All required python modules are present!",fill=TRUE)
  }
}

#' @export
#'
py_install_CompInt <-
  function(...,
           envname = ifelse(reticulate::virtualenv_exists("r-CompInt"),"r-CompInt","r-reticulate"),
           new_env = identical(envname, "r-CompInt")) {

    if(new_env && virtualenv_exists(envname)){
      virtualenv_remove(envname)}

    reticulate::py_install(packages = c("torch","torchquad"), envname = envname, ...)
  }


