
#' @export
get_gME<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,catRIbin=FALSE,...){
eval(ChunkList$getting_situated)

  if(is.null(reg_of_interest)){stop("In order to calculate gME values, a regressor of interest must be defined using the variable reg_of_interest.")}
  if(!reg_of_interest%in%regs(model)){stop("'reg_of_interest' must be a variable used for the model specification.")}


  if(model[["type"]] %in% c("GLM","GLMM")){
    linear_predictor<-make_linear_predictor(mod=model,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)

    eval(ChunkList$getting_inverse)
    eval(ChunkList$preparing_g_theta_calc)

if(integration=="empirical"){
      eval(ChunkList$empirical_Int_catmet_handling)
      eval(ChunkList$data_asmpt__plus__coef_draws)

      if(continue_metric){
        result <- empirical_gME_per_draw(model,linear_predictor,param_draws=coef_draws,EmpDat,reg_of_interest,"met",inverse_link,make_result_LinPred_emp=make_result_LinPred_emp,assumption=assumption)

        #attach_silent_wrapper(data=EmpDat,code=paste0("
        #result<-numeric()
        #  for(i in 1:nrow(coef_draws)){
        #    RI<-torch_tensor(EmpDat[,which(names(EmpDat)==reg_of_interest)],requires_grad=TRUE)
        #    interim<-eval_g_theta_at_point(theta=coef_draws[i,],l=1:nrow(EmpDat),",reg_of_interest,"=RI)
        #    interim$retain_grad
        #    interim$backward(gradient=torch_tensor(rep(1,nrow(EmpDat))))
        #    result[i]<-sum(as.numeric(RI$grad))/nrow(EmpDat)
        #  }")
        #)
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
        IE_refcat<-simple_emp_int(data=cbind(RIvals[[ref_cat]],EmpDat),coef_draws=coef_draws,f=eval_g_theta_at_point)
        ")
        for(cat in nonref_cats){
        attach_silent_wrapper(data=cbind(RIvals[[cat]],EmpDat),code="
        result[cat,]<-simple_emp_int(data=cbind(RIvals[[cat]],EmpDat),coef_draws=coef_draws,f=eval_g_theta_at_point)-IE_refcat
        ")
        }}else{# now for assumption "A.II''"
          all_cats<-c(ref_cat,nonref_cats)
          attach_silent_wrapper(data=cbind(RIvals[[ref_cat]],EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0),]),code="
          IE_refcat<-simple_emp_int(data=cbind(RIvals[[ref_cat]],EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0),]),coef_draws=coef_draws,f=eval_g_theta_at_point)
          ")
          for(cat in nonref_cats){
            other_cats<-all_cats[all_cats != cat]
            attach_silent_wrapper(data=cbind(RIvals[[cat]],EmpDat[which(rowSums(EmpDat[other_cats]) == 0),]),code="
            result[cat,]<-simple_emp_int(data=cbind(RIvals[[cat]],EmpDat[which(rowSums(EmpDat[other_cats]) == 0),]),coef_draws=coef_draws,f=eval_g_theta_at_point)-IE_refcat
            ")
          }
        }
      }
      return(result)
      }
###ADD non empirical integration 'TOFIX !!!
  }

}
