assign_to_parent <- function(name,value){
  assign(name,value,envir = parent.frame(2))
}

process_ellipsis_distributions <- function(...) {
  continuous <- list()
  discrete <- list()

  ellipsis_list <- list(...)
  modind <- which(names(ellipsis_list)=="model")
  model <- ellipsis_list[[modint]]
  regressor_list <- ellipsis_list[-modint]

  if(length(regressor_list)==0){return(NULL)
  }else{
######CONTINUE HERE!!!!!

  # Iterate over the elements of the ellipsis list
  for (name in names(regressor_list)) {
    # Extract the type and int values from the ellipsis list
    entry <- ellipsis_list[[name]]
    type <- entry$type
    int <- entry$int

    # Based on the type, add the entry to the appropriate list
    if (type == "continuous") {
      continuous[[name]] <- int
    } else if (type == "discrete") {
      discrete[[name]] <- int
    } else {
      # Handle invalid type (optional)
      warning(paste("Invalid type for", name, ": ", type))
    }
  }

return(list(discrete=discrete,continuous=continuous))
}
}

################################################################################

#' @export
empirical <- function(assumption,newdata = NULL,subset=NULL){
  returnfunction <- function(...) {
    assign_to_parent("integration","empirical")
    assign_to_parent("assumption",assumption)
    if (!any(is.null(newdata) & is.null(subset))) {
      if (!("model" %in% names(list(...)))) {
        stop("If at least one of the arguments newdata and subset are not NULL, 'model' must be specified in the ellipsis of 'empirical's return function.")
      }else{
           newdat = newdata_subset_merge(newdata,subset,mod=list(...)$model)
          }
    }else{
           newdat = NULL
    }
    assign_to_parent("newdata",newdat)
  }
  return(returnfunction)
}
################################################################################
#' @export
cont <- function(min,max){
return(list(type="continuous",int=c(min,max)))
}

#' @export
disc <- function(values){
return(list(type="discrete",int=values))
}


################################################################################
#' @export
uniform <- function(assumption){
  returnfunction <- function(...) {
    assign_to_parent("integration","defined_measures")
    assign_to_parent("assumption",assumption)
    if (!("model" %in% names(list(...)))) {
      stop("'model' needs to be specified.")
    }
  }
  return(returnfunction)
}






