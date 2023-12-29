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

pointwise_gME <- function(mod, point){
  reticulate::source_python("inst/python_scripts/InverseFunctions.py")
  reticulate::source_python("inst/python_scripts/gME_calculations.py")
}








