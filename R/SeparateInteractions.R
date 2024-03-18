make_interaction_data <- function(data, separate_interactions){
  if(!separate_interactions){
    return(data)
  }else{
    if (!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]) {
      stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")
    }

    terms <- mod[["model_specification"]][["regs"]][["interactions"]][["terms"]]
    notation <- mod[["model_specification"]][["regs"]][["interactions"]][["notation"]]

    INTs <- data.frame(
      terms = stringr::str_replace(terms, paste0("\\", notation), "_x_"),
      expr = stringr::str_replace(terms, paste0("\\", notation), "*")
    )

    for (i in seq_along(INTs$terms)) {
      data[[INTs$terms[i]]] <- with(data, {
        eval(parse(text = INTs$expr[i]))
      })
    }
    return(data)
  }
}


#if (separate_interactions) {
#if (!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]) {
#  stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")
#}

#terms <- mod[["model_specification"]][["regs"]][["interactions"]][["terms"]]
#notation <- mod[["model_specification"]][["regs"]][["interactions"]][["notation"]]

#INTs <- data.frame(
#  terms = stringr::str_replace(terms, paste0("\\", notation), "_x_"),
#  expr = stringr::str_replace(terms, paste0("\\", notation), "*")
#)

#for (i in seq_along(INTs$terms)) {
#  dat[[INTs$terms[i]]] <- with(dat, {
#    eval(parse(text = INTs$expr[i]))
#  })
#}
#}
