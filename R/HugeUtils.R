#make_linear_predictor

make_linear_predictor<-function(mod,reg_of_interest=NULL,separate_interactions=FALSE){
  check_model_class(mod,"mod")

  coefs<-coef(mod)
  if(is.null(names(coefs))){stop("The 'coefs' entry of the CompInt model-object is unnamed.")}

  model_coefficients<-paste0("theta[",1:length(coefs),"]")

  if(mod[["model_specification"]][["intercept"]]){
    if(!grepl("tercept",names(coefs)[1])){warning("The first coefficient's name does not contain the term intercept, but is being used as intercept anyways.")}
    model_terms <- list(list(model_term=model_coefficients[1]))
    start<-2
  }else{model_terms <- 0
  start<-1}

  for (i in start:length(coefs)) {
    if(start==2){j=i}else{j=i+1}
    INT_notation<-paste0("\\",mod[["model_specification"]][["regs"]][["interactions"]][["notation"]])

    indexing <- "[l]"
    REG<-names(coefs)[i]
    TERM<-paste0(model_coefficients[i], " * ", REG,indexing)
    if(separate_interactions & !is.null(reg_of_interest)){
      if(grepl(reg_of_interest,TERM)){TERM<-stringr::str_replace(TERM,INT_notation,"_x_")
      }else{
        TERM<-stringr::str_replace(TERM,INT_notation,paste0(indexing," * "))
      }
    }else{TERM<-stringr::str_replace(TERM,INT_notation,paste0(indexing," * "))}
    model_terms[[j]] <- list(model_term = TERM,
                             regressors = unlist(strsplit(REG,INT_notation)),
                             coefficient = model_coefficients[i],
                             categorical_element = unlist(sapply(unlist(strsplit(REG,INT_notation)),function(x)wich_reg_is_involved("categorical",mod,x))),
                             metric_element = unlist(sapply(unlist(strsplit(REG,INT_notation)),function(x)wich_reg_is_involved("metric",mod,x)))
    )}

  linpred_novec <- merge_linpred_terms(model_terms)

  #The following would be a good check, but we have to check that it does not clash with make_binary
  #if(!identical(sort(unique(unlist(listels_by_name(model_terms, "categorical_element")))), sort(mod[["model_specification"]][["regs"]][["categorical"]])))){
  #stop("")}

  if(!length(mod[["model_specification"]][["regs"]][["categorical"]])==0){
    catTERMS<-listels_by_name(model_terms,"categorical_element")
    index<-which(lengths(catTERMS)>0)
    catTERMS<-catTERMS[index]

    vec_groups<-merge_vectors(create_CatInt_groups(catTERMS))
  }else{
    vec_groups<-NULL
  }

  SepEls<-unlist(lapply(listels_by_name(model_terms,"model_term"),function(x)strsplit(x,"\\*")), recursive = FALSE)
  non_thetas<-lapply(SepEls, function(x) x[-1])
  non_thetas[which(lengths(non_thetas)==0)]<-"1"

  mat_result <- list_to_vecmat(non_thetas,vec_groups,indexing)

  M <- mat_result[[1]]
  reg_groups <- mat_result[[2]]

  #VecSum<-vectorized_sum(v1=lapply(SepEls,function(x) trimws(x[[1]])),
  #veclist=lapply(seq_len(ncol(M)), function(i) M[, i]))

  linear_predictor <- list(vectorized=list(thetas=unlist(lapply(SepEls,function(x) trimws(x[[1]]))),
                                           matrix=M), reg_groups = reg_groups,
                           non_vectorized=linpred_novec)


  return(linear_predictor)
}

#Calculate gME for one point

empirical_gME_per_draw <- function(mod, LinPred, param_draw, data, reg_of_interest, cat_or_met, inverse_link,make_result_LinPred_emp){
  rownames(data) <- NULL
  points <- apply(data,1,function(x)vec = as.data.frame(t(c(x))))
  vec_list <- lapply(LinPred$reg_groups,function(x)list(x))
  Mat <- LinPred[["vectorized"]][["matrix"]]

  if(cat_or_met=="cat"){
    ### THIS IS THE PART THAT NEEDS TO BE CHANGED FOR INTERACTIONS #TOFIX
    RIvals_prep<-dealing_with_catRI(dat=point,RIcat_raw=RIcat_raw,g_theta=LinPred$non_vectorized,RIname=reg_of_interest)
    RIvals<-RIvals_prep[["vals"]]
    ref_cat<-RIvals_prep[["ref_cat"]]
    nonref_cats<-RIvals_prep[["nonref_cats"]]
    RIname <- nonref_cats
  }else{
  RIname <- reg_of_interest}
  RIentry <- LinPred$reg_groups[[which(unlist(lapply(LinPred$reg_groups,function(x)any(RIname%in% x))))]]

  if(cat_or_met=="met"){
    gMEs<-numeric()
    val_lists <- lapply(points,function(x)replace_values(vec_list, x))
    print(val_lists)
    for(i in seq_along(RIentry)){
      gMEs[i] <- make_result_LinPred_emp(Mat=listify_mat(Mat,1,inner_list = TRUE), vec_list=vec_list,thetas=param_draw,val_lists = val_lists,grad_variable = list(RIentry[i]),fun=reticulate::r_to_py(inverse_link))
    }
    names(gMEs) <- RIentry
  }else{
    gMEs<-numeric()
    for(i in seq_along(RIentry)){
    point_ref <- point_nonref <- point
    point_ref[names(RIvals[[RIentry[i]]])] <- as.data.frame(t(RIvals[[ref_cat]]))
    point_nonref[names(RIvals[[RIentry[i]]])] <- as.data.frame(t(RIvals[[RIentry[i]]]))
    val_list_nonref <- lapply(replace_values(vec_list, point_nonref),as.list)
    val_list_ref <- lapply(replace_values(vec_list, point_ref),as.list)
    gMEs[i] <- make_result_LinPred(Mat=listify_mat(Mat,1,inner_list = TRUE), vec_list=vec_list,thetas=param_draw,val_list = val_list_nonref,val_list2 = val_list_ref,fun=reticulate::r_to_py(inverse_link))
    }
  names(gMEs) <- RIentry
  }
  return(gMEs)
}









