
#' @export
get_gME<-function(model_fit,assumption=NULL,reg_of_interest=NULL,seed=NULL,ndraws=1000,integration="empirical",separate_interactions=FALSE,newdata=NULL,subset=NULL,...){
  assumptionstop(assumption)
  ellipsisvars <- names(list(...))
  for (variablename in ellipsisvars) {
    assign(variablename, list(...)[[variablename]], envir = environment())
  }
  model<-model_transform(model_fit,data=data) #This needs to be extended depending on the model structure functions that are written

  if(model[["type"]] %in% c("GLM","GLMM")){
    if(is.null(reg_of_interest)){stop("In order to calculate gME values, a regressor of interest must be defined using the variable reg_of_interest.")}

    linear_predictor<-make_linear_predictor(mod=model,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)
    if(!"inverse_link"%in%ellipsisvars){
    if(!model[["model_specification"]][["family"]][["Link"]] %in% Inverse.Functions[["Link"]]){
      stop("Unfortunately, there is no default inverse link function for the given model's Link='",model[["model_specification"]][["family"]][["Link"]],"'.\n Please specify the variable 'inverse_link' in the function call")
      }else{inverse_link<-Inverse.Functions[["Inverse"]][which(Inverse.Functions[["Link"]]==model[["model_specification"]][["family"]][["Link"]])]}
    }

    eval_g_theta_at_point<-eval(parse(text=paste("function(theta,RI,l){",
                                make_g_theta(model_type=model[["type"]],linear_predictor=linear_predictor,inverse_link=inverse_link,...)
                                ,"}")))

    if(integration=="empirical"){
      data_asmpt<-data_according_to_assumptions(mod=model,assumption=assumption,newdata=newdata,reg_of_interest=reg_of_interest)
      if(length(model[["model_specification"]][["regs"]][["metric"]])==0){
        EmpDat<-data_asmpt
      }else{EmpDat<-make_dummy_coded_data(mod=model,dat=data_asmpt,reg_of_interest=reg_of_interest,separate_interactions=separate_interactions)}

      coef_draws<-draws_from_paramdist(model=model,ndraws=ndraws,seed=seed,...)

      if(reg_of_interest%in%model[["model_specification"]][["regs"]][["metric"]]){
        result<-numeric()
        with(EmpDat, {
          for(i in 1:nrow(coef_draws)){
            RI<-torch_tensor(data_asmpt[,which(names(data_asmpt)==reg_of_interest)],requires_grad=TRUE)
            interim<-eval_g_theta_at_point(theta=coef_draws[i,],RI=RI,l=1:nrow(data_asmpt))
            interim$retain_grad
            interim$backward(gradient=torch_tensor(rep(1,nrow(data_asmpt))))
            result[i]<-sum(as.numeric(RI$grad))/nrow(data_asmpt)
          }
        })
        return(result)
      }

      }
###ADD non empirical integration 'TOFIX !!!
  }

}
