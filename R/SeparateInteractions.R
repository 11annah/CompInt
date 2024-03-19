make_interaction_data <- function(mod, data, reg_of_interest, separate_interactions){
  if(!separate_interactions){
    return(list(data=data, involved=reg_of_interest))
  }else{
    if (!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]) {
      stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")
    }

    if(reg_of_interest %in% mod[["model_specification"]][["regs"]][["metric"]]){
    all_int_terms <- mod[["model_specification"]][["regs"]][["interactions"]][["terms"]]
    }else{
    all_int_terms <- grep( paste0("\\", notation),names(coef(mod)))
    }

    terms <- all_int_terms[grep(reg_of_interest,all_int_terms)]
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

    involved_regs <- c(INTs$terms,unique(strsplit(terms,paste0("\\", notation))))
    return(list(data=data, involved=unlist(involved_regs)))
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
