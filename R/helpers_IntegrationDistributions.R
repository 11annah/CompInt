assign_to_parent <- function(name,value,pos=2){
  assign(name,value,envir = parent.frame(pos))
}

process_single_dist <- function(dist){
  execute <- dist[["output"]]
  for(arg in names(dist[["args"]])){
    assign_to_parent(arg,dist[["args"]][[arg]])
  }
  environment(execute) <- parent.frame()
  execute(model=model)
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
