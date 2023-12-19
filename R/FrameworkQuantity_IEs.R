#' @export
get_IE<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,catRIbin=FALSE,...){
  eval(ChunkList$getting_situated)

  if(!(reg_of_interest%in%regs(model) | is.null(reg_of_interest))){stop("'reg_of_interest' must be a variable used for the model specification.")}

  if(model[["type"]] %in% c("GLM","GLMM")){


  }


}
