#' @export
assumption1 <- function(dist){
  returnfunction <- function(model,...) {
  assign_to_parent("assumption","A.I")
    if(class(dist)=="all_empirical"){
      process_single_dist(dist)
    }
  }
  return(returnfunction)
}

#' @export
assumption2 <- function(dist){
  returnfunction <- function(model,...) {
  assign_to_parent("assumption","A.II'")
    if(class(dist)=="all_empirical"){
      process_single_dist(dist)
    }
  }
  return(returnfunction)
}

#' @export
assumption3 <- function(dist){
  returnfunction <- function(model,...) {
  assign_to_parent("assumption","A.II''")
    if(class(dist)=="all_empirical"){
      process_single_dist(dist)
    }
  }
  return(returnfunction)
}

################################################################################

#' @export
all_empirical <- function(newdata = NULL,subset=NULL){
  returnfunction <- function(...) {
    assign_to_parent("distribution","empirical")
    if (!any(is.null(newdata) & is.null(subset))) {
      if (!("model" %in% names(list(...)))) {
        stop("If at least one of the arguments newdata and subset are not NULL, 'model' must be specified in the ellipsis of 'empirical's return function.")
      }else{
           newdat = newdata_subset_merge(newdata,subset,mod=list(...)$model)
          }
    }else{
           newdat = NULL
    }
    browser()
    assign_to_parent("newdata",newdat)
  }
  return(structure(list(output=returnfunction,args=list(newdata=newdata,subset=subset)),class="all_empirical"))
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
uniform <- function(){
  returnfunction <- function(...) {
    assign_to_parent("distribution","defined_measures")
    if (!("model" %in% names(list(...)))) {
      stop("'model' needs to be specified.")
    }
  }
  return(returnfunction)
}






