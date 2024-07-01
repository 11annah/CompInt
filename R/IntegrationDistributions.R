#' Functions for Assumptions
#'
#' These functions implement different assumptions for a model.
#'
#' @description
#' The functions assumption1, assumption2, and assumption3 implement different assumptions for a model, namely A.I, A.II', and A.II'', respectively.
#'
#' @param dist The distribution parameter.
#' @param model The model object.
#' @param ... Additional arguments to be passed to other functions.
#'
#' @return A function with specified assumption for the model.
#'
#' @details
#' Each function assigns the specified assumption to the parent environment and processes predefined measures if available.
#'
#' @rdname assumptions
#' @export
assumption1 <- function(dist) {
  returnfunction <- function(model, ...) {
    assign_to_parent("assumption", "A.I")
    if (any(class(dist) %in% "predefined_measures")) {
      process_predefined_measures(dist)
    }
  }
  return(returnfunction)
}

#' @rdname assumptions
#' @export
assumption2 <- function(dist) {
  returnfunction <- function(model, ...) {
    assign_to_parent("assumption", "A.II'")
    if (any(class(dist) %in% "predefined_measures")) {
      process_predefined_measures(dist)
    }
  }
  return(returnfunction)
}

#' @rdname assumptions
#' @export
assumption3 <- function(dist) {
  returnfunction <- function(model, ...) {
    assign_to_parent("assumption", "A.II''")
    if (any(class(dist) %in% "predefined_measures")) {
      # TOFIX: extend following line
      if (!any(class(dist) %in% "all_empirical")) {
        stop("The selected predefined measure is not compatible with the chosen assumption.")
      }
      process_predefined_measures(dist)
    }
  }
  return(returnfunction)
}

################################################################################

#' @export
all_empirical <- function(newdata = NULL, subset = NULL) {
  returnfunction <- function(model, pos, ...) {
    assign_to_parent("distribution", "empirical", pos = pos)
    if (!any(is.null(newdata) & is.null(subset))) {
      newdat <- newdata_subset_merge(newdata, subset, mod = list(...)$model)
    } else {
      newdat <- NULL
    }
    assign_to_parent("newdata", newdat, pos = pos)
  }
  return(structure(list(output = returnfunction, args = list(newdata = newdata, subset = subset)), class = c("predefined_measures", "all_empirical")))
}

#' @export
RIunif_empirical <- function(min, max, newdata = NULL, subset = NULL) {
  returnfunction <- function(model, pos, ...) {
    assign_to_parent("distribution", "other_standard_opts", pos = pos)
    if (!any(is.null(newdata) & is.null(subset))) {
      newdat <- newdata_subset_merge(newdata, subset, mod = list(...)$model)
    } else {
      newdat <- NULL
    }
    assign_to_parent("newdata", newdat, pos = pos)
    assign_to_parent("ints.RIunif_empirical", list(RI = c(min, max)), pos = pos)
  }
  return(structure(list(output = returnfunction, args = list(min = min, max = max, newdata = newdata, subset = subset)), class = c("predefined_measures", "RIunif_empirical")))
}


################################################################################


################################################################################

# uniform <- function(){
# .....
# }
