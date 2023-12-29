
#' @export
get_gME<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,catRIbin=FALSE,...){
eval(ChunkList$getting_situated)

  if(is.null(reg_of_interest)){stop("In order to calculate gME values, a regressor of interest must be defined using the variable reg_of_interest.")}
  if(!reg_of_interest%in%regs(model)){stop("'reg_of_interest' must be a variable used for the model specification.")}


  if(model[["type"]] %in% c("GLM","GLMM")){
    linear_predictor<-make_linear_predictor(mod=model,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)

    eval(ChunkList$getting_inverse)

    eval_g_theta_at_point<-eval(parse(text=paste("function(theta,l,",reg_of_interest,"=NULL){",
                                make_g_theta(model_type=model[["type"]],linear_predictor=linear_predictor,inverse_link=inverse_link,vectorized=FALSE,...)
                                ,"}")))

if(integration=="empirical"){
      regsM<-model[["model_specification"]][["regs"]][["metric"]]
      regsC<-model[["model_specification"]][["regs"]][["categorical"]]

      Check<-if(is.null(newdata)){model[["data"]]}else{newdata}# potentially #TOFIX when using 'subset'!
      is.bin<-binary_regs(Check, col=reg_of_interest, catRIbin = catRIbin)
      if(!is.null(is.bin)){
        newdata<-is.bin
        regsM<-regsM[-which(regsM==reg_of_interest)]
        regsC<-c(regsC,reg_of_interest)
      }

      continue_metric<-reg_of_interest%in%regsM
      continue_categorical<-reg_of_interest%in%regsC

      if(continue_categorical){RItype <- "categorical"
      }else{RItype <- "metric"}


      data_asmpt<-data_according_to_assumptions(mod=model,assumption=assumption,newdata=newdata,reg_of_interest=reg_of_interest,RItype=RItype)
      if(length(regsC)==0 | is.null(data_asmpt)){
        EmpDat<-data_asmpt
      }else{
        if(regsC==reg_of_interest & assumption %in% c("A.I","A.II'")){EmpDat<-data_asmpt}else{
        EmpDat<-make_dummy_coded_data(mod=model,dat=data_asmpt,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)}}

      coef_draws<-draws_from_paramdist(model=model,ndraws=ndraws,seed=seed,...)

      if(continue_metric){
        attach_silent_wrapper(data=EmpDat,code=paste0("
        result<-numeric()
          for(i in 1:nrow(coef_draws)){
            RI<-torch_tensor(EmpDat[,which(names(EmpDat)==reg_of_interest)],requires_grad=TRUE)
            interim<-eval_g_theta_at_point(theta=coef_draws[i,],l=1:nrow(EmpDat),",reg_of_interest,"=RI)
            interim$retain_grad
            interim$backward(gradient=torch_tensor(rep(1,nrow(EmpDat))))
            result[i]<-sum(as.numeric(RI$grad))/nrow(EmpDat)
          }")
        )
      }

      if(continue_categorical){
        if("refcat" %in% ellipsisvars){
          #TOFIX #Code for when the RI's reference category should be one that is not specified in the model
        }
        RIvals_prep<-dealing_with_catRI(dat=EmpDat,RIcat_raw=RIcat_raw,g_theta=eval_g_theta_at_point,RIname=reg_of_interest)
        RIvals<-RIvals_prep[["vals"]]
        ref_cat<-RIvals_prep[["ref_cat"]]
        nonref_cats<-RIvals_prep[["nonref_cats"]]

        result<-matrix(nrow=length(nonref_cats),ncol=ndraws)
        rownames(result)<-nonref_cats

        if(assumption %in% c("A.I","A.II'")){
        attach_silent_wrapper(data=cbind(RIvals[[ref_cat]],EmpDat),code="
        IE_refcat<-categorical_regressor_draws(data=cbind(RIvals[[ref_cat]],EmpDat),coef_draws=coef_draws,f=eval_g_theta_at_point)
        ")
        for(cat in nonref_cats){
        attach_silent_wrapper(data=cbind(RIvals[[cat]],EmpDat),code="
        result[cat,]<-categorical_regressor_draws(data=cbind(RIvals[[cat]],EmpDat),coef_draws=coef_draws,f=eval_g_theta_at_point)-IE_refcat
        ")
        }}else{# now for assumption "A.II''"
          all_cats<-c(ref_cat,nonref_cats)
          attach_silent_wrapper(data=cbind(RIvals[[ref_cat]],EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0),]),code="
          IE_refcat<-categorical_regressor_draws(data=cbind(RIvals[[ref_cat]],EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0),]),coef_draws=coef_draws,f=eval_g_theta_at_point)
          ")
          for(cat in nonref_cats){
            other_cats<-all_cats[all_cats != cat]
            attach_silent_wrapper(data=cbind(RIvals[[cat]],EmpDat[which(rowSums(EmpDat[other_cats]) == 0),]),code="
            result[cat,]<-categorical_regressor_draws(data=cbind(RIvals[[cat]],EmpDat[which(rowSums(EmpDat[other_cats]) == 0),]),coef_draws=coef_draws,f=eval_g_theta_at_point)-IE_refcat
            ")
          }
        }
      }
      return(result)
      }
###ADD non empirical integration 'TOFIX !!!
  }

}
