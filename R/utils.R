assumptionstop <- function(assumption) {
  if (is.null(assumption)) {
    stop("TBD #TOFIX ")
  }
}

check_model_class <- function(model, inputname) {
  message <- paste0("Input ", inputname, " is not of the expected class (CompInt_model).")
  if (!inherits(model, "CompInt_model")) {
    stop(message)
  }
}


reg_naming_check <- function(mod) {
  regs <- regs(mod)
  contained <- sapply(regs, function(element1) any(sapply(regs, function(element2) grepl(element1, element2) && element1 != element2)))
  if (any(contained)) {
    stop(paste0("The name of the regressor(s) '", paste(regs[contained], collapse = " & "), "' is fully contained within another regressor name.\nUnfortunately, this is incompatible with the CompInt package's functionality.\nPlease rename the regressor(s) accordingly."))
  }
}


attach_silent_wrapper <- function(data, code) {
  for (key in colnames(data)) {
    assign(key, data[[key]], envir = parent.frame())
  }
  eval(parse(text = code), envir = parent.frame())

  rm(list = colnames(data), envir = parent.frame())
}




data_prep <- function(mod, data = NULL, separate_interactions = FALSE) {
  if (is.null(data)) {
    if(!is.null(mod[["data"]])){
    data <- mod[["data"]]
    }else{
      data <- expand.grid(mod[["catreg_list"]])
    }
  }
  dat <- data[, regs(mod), drop = FALSE]
  dat <- dat[complete.cases(dat), , drop = FALSE]

  return(dat)
}

newdata_subset_merge <- function(newdata, subset, mod) {
  if (is.null(subset)) {
    return(newdata)
  } else {
    if (is.null(newdata)) {
      newdata <- mod[["data"]]
    }
    output <- newdata[subset, ]
    return(output)
  }
}



##
data_according_to_assumptions <- function(mod, assumption = NULL, newdata = NULL, reg_of_interest = NULL, RItype = "metric") {
  assumptionstop(assumption)
  if (!RItype %in% c("metric", "categorical")) {
    stop("The variable 'RItype' gives the data type of the regressor of interest.\nTherefore,it has to be equal to either 'metric' or 'categorical'.")
  }

  prep_for_asmpt <- data_prep(mod, data = newdata)
  if (assumption == "A.II''") {
    data_asmpt <- prep_for_asmpt
  }

  if (RItype == "categorical") {
    #assign("RIcat_raw", prep_for_asmpt[[reg_of_interest]], envir = parent.frame())
    #if (ncol(prep_for_asmpt) == 1 & assumption %in% c("A.I", "A.II'")) {
    #  return(NULL)
    } #else {
    #}
  #} else {
    #assign("RIcat_raw", NULL, envir = parent.frame())
  #}

  if (assumption == "A.I") {
    if (ncol(prep_for_asmpt) == 1) {
      data_asmpt <- prep_for_asmpt
    } else {
      if(RItype == "categorical"){categories <- levels(as.factor(prep_for_asmpt[[reg_of_interest]]))
        prep_for_asmpt[[reg_of_interest]] <- NULL}
      data_asmpt <- as.data.frame(do.call(expand.grid, prep_for_asmpt))
      if(RItype == "categorical"){data_asmpt <-cbind(rep(categories,each=ceiling(nrow(df) / length(vec)))[1:nrow(data_asmpt)],data_asmpt)
      names(data_asmpt)[1] <- reg_of_interest
      }
    }
    names(data_asmpt) <- names(prep_for_asmpt)
  }

  if (assumption == "A.II'") {
    if (is.null(reg_of_interest)) {
      stop("A regressor of interest needs to be specified for assumption A.II'")
    }
    if (ncol(prep_for_asmpt) == 1|RItype == "categorical") {
      data_asmpt <- prep_for_asmpt
    } else {
      RI <- prep_for_asmpt[, which(names(prep_for_asmpt) == reg_of_interest)]
      data_asmpt <- as.data.frame(cbind(RI, prep_for_asmpt[rep(seq_len(nrow(prep_for_asmpt)), each = length(RI)),-which(names(prep_for_asmpt) == reg_of_interest), drop = FALSE]))
      names(data_asmpt)[1] <- reg_of_interest
    }
  }

  return(data_asmpt)
}



make_dummy_coded_data <- function(mod, dat, reg_of_interest = NULL, separate_interactions = FALSE) {
  check_model_class(mod, "mod")
  if (any(!complete.cases(dat))) {
    stop("The data provided for empirical integration contains NA values.")
  }
  if (isFALSE(separate_interactions)) {
    cat_vars <- mod[["model_specification"]][["regs"]][["categorical"]]
  } else {
    # TOFIX!!
  }
  DCdat <- dat
  for (i in seq_along(cat_vars)) {
    if (!cat_vars[i] %in% names(DCdat)) {
      stop(paste0("Variable '", cat_vars[i], "' not found in the data frame of observations."))
    }
    DCdat <- cbind(DCdat, model.matrix(~ 0 + as.factor(DCdat[[cat_vars[i]]])))
    DCdat <- DCdat[, !names(DCdat) %in% cat_vars[i], drop = FALSE]
    names(DCdat) <- sub("as.factor\\(DCdat\\[\\[cat_vars\\[i\\]\\]\\])", cat_vars[i], names(DCdat))
  }
  return(DCdat)
}


binary_regs <- function(data, col, catRIbin) {
  if (!is.null(col)) {
    if (is.numeric(data[[col]])) {
      # Check for values other than NA, 0, and 1
      unique_values <- unique(data[[col]][!is.na(data[[col]])])
      if (all(unique_values %in% c(0, 1))) {
        if (!catRIbin) {
          prompt <- paste0(
            "The regressor '", col, "' is stored as a numeric dichotomous variable.\nAs such, it could be treated as either a categorical or metric regressor.\nIf '", col, "' cannot take values than '0', '1', or 'NA', it is advisable to convert it to a categorical variable.\nOtherwise, it makes sense to keep it as numeric.\n
Enter C for converting to categorical and N for keeping the variable numeric: "
          )
          user_input <- readline(prompt = cat(prompt,fill=TRUE))
          if (!as.character(user_input) %in% c("C", "N")) {
            message("Sorry, but you have to decide between C and N.")
            user_input <- readline(prompt = cat(prompt,fill=TRUE))
          }
          if (!as.character(user_input) %in% c("C", "N")) {
            stop("Sorry, but you really have to decide between C and N.")
          }
        }
        if (as.character(user_input) == "C" | catRIbin) {
          data[, col] <- data[, col]
          names(data)[which(names(data) == col)] <- paste0(col, "1")
          data[, paste0(col, "0")] <- abs(as.numeric(data[, paste0(col, "1")]) - 1)
          return(data = data)
        }
      }
    }
  }
}

dealing_with_catRI <- function(dat, g_theta, RIname = "RI", separate_interactions=FALSE) {
  if(separate_interactions & length(RIname)==1){
    stop("If separate interactions are to be calculated, the length of the 'RIname' argument must be greater than zero.")
  }
  if(!separate_interactions){
  all_cats <- names(dat)[grepl(RIname, names(dat))]
  nonref_cats <- names(which(sapply(all_cats, function(x) grepl(x, paste(deparse(g_theta), collapse = "")))))
  }
  if(separate_interactions){
    all_catregs <- mod[["model_specification"]][["regs"]][["categorical"]]
    catregs <- all_catregs[which(unlist(sapply(all_catregs,function(x)any(grepl(x,RIname)))))]
    all_cats <- names(dat)[which(unlist(sapply(names(dat),function(x)grepl(paste(catregs, collapse = "|"),x))))]
    nonref_cats <- names(which(sapply(all_cats, function(x) grepl(x, paste(deparse(g_theta), collapse = "")))))
    }


  init <- data.frame(matrix(0, nrow = 1, ncol = length(nonref_cats)))
  names(init) <- nonref_cats

  if(!separate_interactions){
  vals <- replicate(n = length(all_cats), init, simplify = FALSE)
  names(vals) <- all_cats
  rep1 <- mapply(function(x, y) which(names(x) %in% y), vals, names(vals))

  for (cat in names(vals)) {
    vals[[cat]][1, rep1[[cat]]] <- 1
  }
  return(list(list(vals = vals, ref_cat = all_cats[!(all_cats %in% nonref_cats)], nonref_cats = nonref_cats)))
  }
  if(separate_interactions){

  }

}

simple_emp_int <- function(data, coef_draws, f) {
  environment(f) <- environment()
  attach_silent_wrapper(data = data, code = "
output <- apply(coef_draws,1,function(x){sum(f(theta=x,l=1:nrow(data)))/nrow(data)})")
  return(output)
}


wich_reg_is_involved <- function(met_or_cat, mod, term) {
  logical <- unlist(sapply(mod[["model_specification"]][["regs"]][[met_or_cat]], function(y) grepl(y, term)))
  if (is.null(logical)) {
    return(NULL)
  } else {
    mod[["model_specification"]][["regs"]][[met_or_cat]][which(logical)]
  }
}


listels_by_name <- function(list, name) {
  lapply(list, `[[`, name)
}

remove_contained_lists <- function(list_of_lists) {
  result <- list_of_lists
  ind <- numeric()
  for (i in seq_along(list_of_lists)) {
    for (j in seq_along(list_of_lists)) {
      if (i != j & list_contains(list_of_lists[[i]], list_of_lists[[j]])) {
        ind <- c(ind, i)
      }
    }
  }
  if (length(ind) != 0) {
    result <- result[-ind]
  }
  return(result)
}

create_CatInt_groups <- function(vectors) {
  char_entries <- unique(unlist(vectors))
  groups <- vector("list", length = length(char_entries))

  for (i in seq_along(char_entries)) {
    char_entry <- char_entries[i]
    groups[[i]] <- vectors[sapply(vectors, function(vec) char_entry %in% vec)]
  }

  groups <- unique(lapply(do.call(c, groups[lengths(groups) > 0]), unname))

  unique_groups <- remove_contained_lists(groups)

  return(unique_groups)
}


merge_vectors <- function(list_of_vectors) {
  result <- list_of_vectors
  merged <- logical(length(list_of_vectors))

  for (i in seq_along(list_of_vectors)) {
    if (!merged[i]) {
      for (j in seq_along(list_of_vectors)) {
        if (i != j && any(list_of_vectors[[i]] %in% list_of_vectors[[j]])) {
          result[[i]] <- union(result[[i]], result[[j]])
          merged[j] <- TRUE
        }
      }
    }
  }

  result <- result[!merged]
  return(result)
}


merge_linpred_terms <- function(termlist) {
  termlist <- unlist(listels_by_name(list = termlist, name = "model_term"))
  termlist[2:length(termlist)] <- paste0("+ ", termlist[2:length(termlist)])
  return(paste(termlist, collapse = " "))
}


merge_cols <- function(Mat, indexing) {
  result <- character(nrow(Mat))
  regnames <- character()
  for (i in seq_along(result)) {
    if (all(Mat[i, ] == "1")) {
      result[i] <- "1"
    } else {
      result[i] <- paste(Mat[i, which(Mat[i, ] != "1")], collapse = "*")
      regnames <- unique(c(regnames, unname(sapply(trimws(Mat[i, which(Mat[i, ] != "1")]), function(x) gsub_complex(indexing, x)))))
    }
  }
  return(list(result, regnames))
}

list_to_vecmat <- function(list, groups, indexing) {
  Mat <- matrix("1", ncol = length(unlist(list)), nrow = length(list))
  regname_list <- list()
  for (i in 1:nrow(Mat)) {
    if (i != 1) {
      before <- length(unlist(list[1:(i - 1)]))
    } else {
      before <- 0
    }
    entry <- list[[i]]
    for (j in 1:length(entry)) {
      dim2 <- before + j
      Mat[i, dim2] <- trimws(entry[j])
    }
  }

  origMat <- Mat
  if (is.null(groups)) {
    result <- Mat
  } else {
    for (i in 1:length(groups)) {
      merge_ind <- which(apply(Mat, 2, function(x) any(sapply(groups[[i]], function(v) grepl(v, x)))))
      ToMerge <- Mat[, merge_ind, drop = FALSE]
      merge_cols_res <- merge_cols(ToMerge, indexing)
      regname_list[[i]] <- merge_cols_res[[2]]
      Mat <- cbind(Mat[, -merge_ind, drop = FALSE], merge_cols_res[[1]])
    }
  }
  orig_cols <- which(apply(Mat, 2, function(col) any(apply(origMat, 2, function(check) all(col == check)))))
  if (length(orig_cols) > 0) {
    cols <- unique(c(Mat[, orig_cols]))
    cols <- sapply(cols[-which(cols == "1")], function(x) gsub_complex(indexing, x))
    for (i in seq_along(cols)) {
      if (length(col) > 0) {
        regname_list <- c(list(cols[i]), regname_list)
      }
    }
  }
  regname_list <- lapply(regname_list, unname)
  result <- apply(Mat, c(1, 2), function(x) gsub_complex(indexing, x))
  return(list(result, regname_list))
}



replace_values <- function(char_list, row_values) {
  return_list <- list()
  attach_silent_wrapper(data = as.data.frame(row_values), code = "
                        for(i in 1:length(char_list)){
                        return_list[[i]] <- list()
                        for(j in 1:length(unlist(char_list[[i]]))){
                        return_list[[i]][[j]] <- eval(parse(text=
                        paste0('c(',paste(unlist(char_list[[i]])[j],collapse=','),')')
                        ))
                        names(return_list[[i]])[j] <- unlist(char_list[[i]])[j]
                        }}")
  return(return_list)
}


prepare_return <- function(matrix,rownames){
  rownames(matrix) <- rownames
  return(matrix)
}





