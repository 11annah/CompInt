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
  returnfunction <- function(model,pos,...) {
    assign_to_parent("distribution","empirical",pos=pos)
    if (!any(is.null(newdata) & is.null(subset))) {
           newdat = newdata_subset_merge(newdata,subset,mod=list(...)$model)
    }else{
           newdat = NULL
    }
    assign_to_parent("newdata",newdat,pos=pos)
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
  #.....
}






