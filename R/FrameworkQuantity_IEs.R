#' @export
get_IE<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,catRIbin=FALSE,...){
  eval(ChunkList$getting_situated)

  if(!(reg_of_interest%in%regs(model) | is.null(reg_of_interest))){stop("'reg_of_interest' must either be a variable used for the model specification or set to NULL.")}

  if(model[["type"]] %in% c("GLM","GLMM")){
    eval(ChunkList$getting_inverse)
    eval(ChunkList$preparing_g_theta_calc)

    if(integration=="empirical"){
      eval(ChunkList$empirical_Int_catmet_handling)
      eval(ChunkList$data_asmpt__plus__coef_draws)

      if(continue_metric){
        attach_silent_wrapper(data=EmpDat, code ="
        result <- simple_emp_int(data=EmpDat,coef_draws=coef_draws,f=eval_g_theta_at_point)
        ")
      }

      if(continue_categorical){

      }

      return(result)
    }
    ###ADD non empirical integration 'TOFIX !!!
  }

}
