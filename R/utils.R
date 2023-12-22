assumptionstop<-function(assumption){
if(is.null(assumption)){stop("TBD #TOFIX ")}
}

check_model_class<-function(model,inputname){
  message<-paste0("Input ",inputname," is not of the expected class (CompInt_model).")
  if(!inherits(model, "CompInt_model")){
    stop(message)
  }
}
regs <- function(object) {
  check_model_class(object,"object")
  return(unlist(object[["model_specification"]][["regs"]][c("metric","categorical")]))
}

reg_naming_check<-function(mod){
regs <- regs(mod)
contained <- sapply(regs, function(element1) any(sapply(regs, function(element2) grepl(element1, element2) && element1 != element2)))
if(any(contained)){stop(paste0("The name of the regressor(s) '",paste(regs[contained], collapse=" & "),"' is fully contained within another regressor name.\nUnfortunately, this is incompatible with the CompInt package's functionality.\nPlease rename the regressor(s) accordingly."))}
}


attach_silent_wrapper <- function(data,code) {
  suppressMessages({suppressWarnings({
    attach(data,warn.conflicts = FALSE)
    eval(parse(text=code), envir = parent.frame())
    on.exit(detach(data))
    })
    })
}




data_prep<-function(mod,data=NULL,separate_interactions=FALSE){
  if(is.null(data)){data<-mod[["data"]]}
  dat <- data[,regs(mod), drop = FALSE]
  dat <- dat[complete.cases(dat),,drop = FALSE]

  if(separate_interactions){
    if(!mod[["model_specification"]][["regs"]][["interactions"]][["present"]]){stop("'seperate_interactions' is specified as TRUE, but are no interaction terms present in the model.")}

    terms<-mod[["model_specification"]][["regs"]][["interactions"]][["terms"]]
    notation<-mod[["model_specification"]][["regs"]][["interactions"]][["notation"]]

    INTs<-data.frame(terms=stringr::str_replace(terms,paste0("\\",notation),"_x_"),
                     expr=stringr::str_replace(terms,paste0("\\",notation),"*"))

    for(i in seq_along(INTs$terms)){dat[[INTs$terms[i]]]<-with(dat,{eval(parse(text=INTs$expr[i]))})}
  }
  return(dat)
}

newdata_subset_merge<-function(newdata,subset,mod){
  if(is.null(subset)){return(newdata)
  }else{
    if(is.null(newdata)){newdata<-mod[["data"]]}
    output<-newdata[subset,]
    return(output)
    }
}



##
data_according_to_assumptions<-function(mod,assumption=NULL,newdata=NULL,reg_of_interest=NULL,RItype="metric"){
  assumptionstop(assumption)
  if(!RItype %in% c("metric","categorical")){stop("The variable 'RItype' gives the data type of the regressor of interest.\nTherefore,it has to be equal to either 'metric' or 'categorical'.")}

  prep_for_asmpt<-data_prep(mod,data=newdata)
  if(assumption=="A.II''"){data_asmpt<-prep_for_asmpt}

  if(RItype=="categorical"){
    assign("RIcat_raw",prep_for_asmpt[[reg_of_interest]],envir = parent.frame())
    if(ncol(prep_for_asmpt)==1 & assumption %in% c("A.I","A.II'")){return(NULL)
    }else{prep_for_asmpt[[reg_of_interest]]<-NULL}
  }else{assign("RIcat_raw",NULL,envir = parent.frame())}

  if(assumption=="A.I"){if(ncol(prep_for_asmpt)==1){data_asmpt<-prep_for_asmpt
  }else{
    data_asmpt<-as.data.frame(do.call(expand.grid,prep_for_asmpt))}
    names(data_asmpt) <- names(prep_for_asmpt)}

  if(assumption=="A.II'"){if(is.null(reg_of_interest)){stop("A regressor of interest needs to be specified for assumption A.II'")}
    if(ncol(prep_for_asmpt)==1){data_asmpt<-prep_for_asmpt
    }else{
      RI<-prep_for_asmpt[,which(names(prep_for_asmpt)==reg_of_interest)]
      data_asmpt<-as.data.frame(cbind(RI,prep_for_asmpt[rep(seq_len(nrow(prep_for_asmpt)), each = length(RI)),-which(names(prep_for_asmpt)==reg_of_interest),drop = FALSE]))
      names(data_asmpt)[1]<-reg_of_interest
    }}

  return(data_asmpt)
}



make_dummy_coded_data<-function(mod,dat,reg_of_interest=NULL,separate_interactions=FALSE){
  check_model_class(mod,"mod")
  if(any(!complete.cases(dat))){
    stop("The data provided for empirical integration contains NA values.")
  }
  if(isFALSE(separate_interactions)){
  cat_vars<-mod[["model_specification"]][["regs"]][["categorical"]]

  }else{
    #TOFIX!!
  }
  DCdat<-dat
  for(i in seq_along(cat_vars)){
    if (!cat_vars[i] %in% names(DCdat)) {
      stop(paste0("Variable '", cat_vars[i], "' not found in the data frame of observations."))
    }
    DCdat <- cbind(DCdat, model.matrix(~0 + as.factor(DCdat[[cat_vars[i]]])))
    DCdat <- DCdat[, !names(DCdat) %in% cat_vars[i], drop = FALSE]
    names(DCdat)<-sub("as.factor\\(DCdat\\[\\[cat_vars\\[i\\]\\]\\])",cat_vars[i],names(DCdat))
  }
  return(DCdat)
}


binary_regs <- function(data, col,catRIbin){
  if(is.numeric(data[[col]])){
    # Check for values other than NA, 0, and 1
    unique_values <- unique(data[[col]][!is.na(data[[col]])])
    if(all(unique_values%in%c(0,1))){
    if(!catRIbin){
    prompt<-paste0(
      "The regressor '",col,"' is stored as a numeric dichotomous variable.\nAs such, it could be treated as either a categorical or metric regressor.\nIf '",col,"' cannot take values than '0', '1', or 'NA', it is advisable to convert it to a categorical variable.\nOtherwise, it makes sense to keep it as numeric.\n
Enter C for converting to categorical and N for keeping the variable numeric: "
    )
    user_input <- readline(prompt = cat(prompt))
    if(!as.character(user_input)%in%c("C","N")){message("Sorry, but you have to decide between C and N.")
      user_input <- readline(prompt = cat(prompt))
    }
    if(!as.character(user_input)%in%c("C","N")){stop("Sorry, but you really have to decide between C and N.")}
    }
    if(as.character(user_input)=="C" | catRIbin){
      data[,col]<-data[,col]
      names(data)[which(names(data)==col)]<-paste0(col,"1")
      data[,paste0(col,"0")]<-abs(as.numeric(data[,paste0(col,"1")])-1)
      return(data=data)
    }
}}}


dealing_with_catRI<-function(dat,RIcat_raw,g_theta,RIname="RI"){
  all_cats <- names(dat)[grepl(RIname,names(dat))]
  if(length(all_cats)==0){
  all_cats<-names(as.data.frame(model.matrix(~0 + as.factor(RIcat_raw))))
  all_cats<-sub("as.factor\\(RIcat_raw\\)",RIname,all_cats)
  }
  nonref_cats <- names(which(sapply(all_cats,function(x)grepl(x,paste(deparse(g_theta),collapse = '')))))

  init<-data.frame(matrix(0, nrow = 1, ncol = length(nonref_cats)))
  names(init)<-nonref_cats
  vals<-replicate(n=length(all_cats),init, simplify = FALSE)
  names(vals)<-all_cats

  rep1<-mapply(function(x,y)which(names(x)%in%y),vals,names(vals))

  for(cat in names(vals)){
    vals[[cat]][1,rep1[[cat]]]<-1
  }

  return(list(vals=vals,ref_cat= all_cats[!(all_cats %in% nonref_cats)],nonref_cats=nonref_cats))

}

categorical_regressor_draws<-function(data,coef_draws,f){
apply(coef_draws,1,function(x){sum(f(theta=x,l=1:nrow(data)))/nrow(data)})
}


wich_reg_is_involved<-function(met_or_cat,mod,term){
  logical<-unlist(sapply(mod[["model_specification"]][["regs"]][[met_or_cat]],function(y)grepl(y,term)))
  if(is.null(logical)){return(NULL)}else{
  mod[["model_specification"]][["regs"]][[met_or_cat]][which(logical)]}
}


listels_by_name<-function(list,name){lapply(list, `[[`, name)}


###
list_contains <- function(list1, list2) {
all(sapply(list1, function(x) any(sapply(list2, function(y) all(x %in% y)))))
}

remove_contained_lists <- function(list_of_lists) {
  result <- list_of_lists
  ind<-numeric()
  for (i in seq_along(list_of_lists)) {
    for (j in seq_along(list_of_lists)) {
      if (i != j & list_contains(list_of_lists[[i]],list_of_lists[[j]])) {ind<-c(ind,i)}
    }
  }
  if(length(ind)!=0){result <- result[-ind]}
  return(result)
}

create_CatInt_groups <- function(vectors) {
  char_entries <- unique(unlist(vectors))
  groups <- vector("list", length = length(char_entries))

  for (i in seq_along(char_entries)) {
    char_entry <- char_entries[i]
    groups[[i]] <- vectors[sapply(vectors, function(vec) char_entry %in% vec)]
  }

  groups <- unique(lapply(do.call(c,groups[lengths(groups) > 0]),unname))

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


merge_linpred_terms <- function(termlist){
termlist<-unlist(listels_by_name(list=termlist,name="model_term"))
termlist[2:length(termlist)]<-paste0("+ ",termlist[2:length(termlist)])
return(paste(termlist,collapse=' '))
}

vec_to_c<-function(vec){
  return(paste0("c(",paste(vec,collapse=","),")"))
}

vectorized_sum<-function(v1,veclist){
  if(length(veclist)==1){return(paste0(vec_to_c(v1),"%*%",vec_to_c(unlist(veclist))))
  }else{
    return(paste0(vec_to_c(v1),"%*%(",paste(lapply(l,vec_to_c),collapse='*'),")"))
    }
}









