#' @export
get_IE<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,catRIbin=FALSE,...){
  eval(ChunkList$getting_situated)

  if(!(any(reg_of_interest%in%regs(model)) | is.null(reg_of_interest))){stop("'reg_of_interest' must either be a variable used for the model specification or set to NULL.")}
  if(is.null(reg_of_interest) & any(assumption %in% c("A.II'"))){stop("In order to calculate individualized expectations under assumptions A.II', a regressor of interest must be defined using the variable reg_of_interest.")}

  if(model[["type"]] %in% c("GLM","GLMM")){
    linear_predictor<-make_linear_predictor(mod=model,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)

    eval(ChunkList$getting_inverse)


    #reticulate::source_python("inst/python_scripts/gME_calculations.py")
    eval_g_theta_at_point<-eval(parse(text=paste("function(theta,l){",
                                                 make_g_theta(model_type=model[["type"]],linear_predictor=linear_predictor,inverse_link=inverse_link,vectorized=FALSE,...)
                                                 ,"}")))

    if(integration=="empirical"){
      eval(ChunkList$empirical_Int_catmet_handling)
      eval(ChunkList$data_asmpt__plus__coef_draws)

      if(continue_metric){
        result <- simple_emp_int(data=EmpDat,coef_draws=coef_draws,f=eval_g_theta_at_point)
      }

      if(continue_categorical){
        if("refcat" %in% ellipsisvars){
          #TOFIX #Code for when the RI's reference category should be one that is not specified in the model
        }
        eval(ChunkList$ prepping_for_catRI)

        result<-matrix(nrow=length(nonref_cats)+1,ncol=ndraws)
        rownames(result)<-c(ref_cat,nonref_cats)

        torem <- setdiff(names(EmpDat), nonref_cats)

        if(assumption %in% c("A.I","A.II'")){
        result[ref_cat,]<-simple_emp_int(data=cbind(RIvals[[ref_cat]],EmpDat[,torem,drop=FALSE]),coef_draws=coef_draws,f=eval_g_theta_at_point)

        for(cat in nonref_cats){
        result[cat,]<-simple_emp_int(data=cbind(RIvals[[cat]],EmpDat[,torem,drop=FALSE]),coef_draws=coef_draws,f=eval_g_theta_at_point)

          }}else{# now for assumption "A.II''"
            all_cats<-c(ref_cat,nonref_cats)
            result[ref_cat,]<-simple_emp_int(data=cbind(RIvals[[ref_cat]],EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0),torem,drop=FALSE]),coef_draws=coef_draws,f=eval_g_theta_at_point)

            for(cat in nonref_cats){
            other_cats<-all_cats[all_cats != cat]
            result[cat,]<-simple_emp_int(data=cbind(RIvals[[cat]],EmpDat[which(rowSums(EmpDat[other_cats]) == 0),torem,drop=FALSE]),coef_draws=coef_draws,f=eval_g_theta_at_point)

            }
          }
      }

      return(result)
    }
    ###ADD non empirical integration 'TOFIX !!!
  }

}
