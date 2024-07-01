#' Calculate Generalized Marginal Effects (gME)
#'
#' This function computes generalized Marginal Effects (gME) for a given model and regressor of interest.
#'
#' @param model_fit A fitted model object.
#' @param reg_of_interest The regressor of interest for which gME values are calculated.
#' @param integration An integration function. Default is NULL. #TOFIX ... \code{\link{assumption1}} \code{\link{assumption2}} \code{\link{assumption3}}
#' @param seed Seed for random number generation. Default is NULL.
#' @param ndraws Number of draws for sampling. Default is 1000.
#' @param separate_interactions Boolean indicating whether interactions should be separated. Default is FALSE.
#' @param catRIbin Boolean indicating categorical RI binning. Default is FALSE.
#' @param ... Additional arguments to be passed to other functions.
#'
#' @return A matrix containing gME values for the regressor of interest.
#'
#' @details This function computes gME values based on the specified model and regressor of interest. It supports various types of models including GLM and GLMM. The gME values are computed either empirically or through other standard options depending on the distribution.
#'
#' @note ...
#'
#' @references
#' Kümpel, H. & Hoffmann, S. A formal framework for generalized reporting methods in parametric settings. ArXiv Prepr. ArXiv221102621 (2022).
#'
#' @export
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ cyl + disp + hp + drat, data = mtcars)
#' get_gME(model_fit = model, reg_of_interest = "cyl")
#' }
get_gME <- function(model_fit, reg_of_interest = NULL, integration = NULL, seed = NULL, ndraws = 10000, separate_interactions = FALSE, catRIbin = FALSE, ...) {
  run_in_parent(getting_situated1)
  integration(model = model)
  run_in_parent(getting_situated2)

  if (is.null(reg_of_interest)) {
    stop("In order to calculate gME values, a regressor of interest must be defined using the variable reg_of_interest.")
  }
  if (!any(reg_of_interest %in% regs(model))) {
    stop("'reg_of_interest' must be a variable used for the model specification.")
  }


  if (model[["type"]] %in% c("GLM", "GLMM")) {
    coef_draws <- draws_from_paramdist(model = model, ndraws = ndraws, seed = seed, ...)

    linear_predictor <- make_linear_predictor(mod = model, reg_of_interest = reg_of_interest, separate_interactions = separate_interactions)

    run_in_parent(getting_inverse)

    eval_g_theta_at_point <- eval(parse(text = paste(
      "function(theta,l,", reg_of_interest, "=NULL){",
      make_g_theta(model_type = model[["type"]], linear_predictor = linear_predictor, inverse_link = inverse_link, vectorized = FALSE, ...),
      "}"
    )))

    if (distribution == "empirical") {
      reticulate::source_python(system.file("python_scripts", "gME_simplegrad.py", package = "CompInt"))
      run_in_parent(empirical_Int_catmet_handling)
      run_in_parent(data_asmpt)

      interaction_data <- make_interaction_data(mod = model, data = EmpDat, reg_of_interest = reg_of_interest, separate_interactions = separate_interactions)
      EmpDat <- interaction_data$data

      if (all(c(any(interaction_data$involved %in% model[["model_specification"]][["regs"]][["metric"]]), any(interaction_data$involved %in% model[["model_specification"]][["regs"]][["categorical"]])))) {
        continue_metric <- FALSE
        continue_categorical <- FALSE
        continue_mixed <- TRUE
      } else {
        continue_mixed <- FALSE
      }


      if (continue_metric) {
        result <- prepare_return(matrix(nrow = length(interaction_data$involved), ncol = ndraws), interaction_data$involved)
        for (reg in interaction_data$involved) {
          # progressr::with_progress({
          # p <- progressr::progressor(along = lapply(nrow(coef_draws), function(x) coef_draws[x,]))
          result[reg, ] <- apply(coef_draws, 1, function(x) {
            simplegrad(
              data = EmpDat,
              LinPred = gsub_complex("[l]", linear_predictor$non_vectorized),
              thetas = c(0, x),
              grad_variable = reg,
              fun = inverse_link
            )
          })
          # })
        }

        # simplegrad(data=EmpDat,LinPred=gsub_complex("[l]",linear_predictor$non_vectorized),thetas=unname(cbind(0,coef_draws)),grad_variable=reg_of_interest,fun=inverse_link)


        # result <- empirical_gME_per_draw(model,linear_predictor,param_draws=coef_draws,EmpDat,reg_of_interest,"met",inverse_link,make_result_LinPred_emp=make_result_LinPred_emp,assumption=assumption)

        # attach_silent_wrapper(data=EmpDat,code=paste0("
        # result<-numeric()
        #  for(i in 1:nrow(coef_draws)){
        #    RI<-torch_tensor(EmpDat[,which(names(EmpDat)==reg_of_interest)],requires_grad=TRUE)
        #    interim<-eval_g_theta_at_point(theta=coef_draws[i,],l=1:nrow(EmpDat),",reg_of_interest,"=RI)
        #    interim$retain_grad
        #    interim$backward(gradient=torch_tensor(rep(1,nrow(EmpDat))))
        #    result[i]<-sum(as.numeric(RI$grad))/nrow(EmpDat)
        #  }")
        # )
      }

      if (continue_categorical) {
        if ("refcat" %in% ellipsisvars) {
          # TOFIX #Code for when the RI's reference category should be one that is not specified in the model
        }
        RIvals_prep <- dealing_with_catRI(dat = EmpDat, g_theta = eval_g_theta_at_point, RIname = reg_of_interest)

        if (!separate_interactions) {
          run_in_parent(prepping_for_catRI, i = 1)
          result <- prepare_return(matrix(nrow = length(nonref_cats), ncol = ndraws), nonref_cats)
        } else {
          # TOFIX
          # continue here!
        }

        torem <- setdiff(names(EmpDat), nonref_cats)

        if (assumption %in% c("A.I", "A.II'")) {
          for (i in seq_along(length(RIvals_prep))) {
            run_in_parent(prepping_for_catRI, i = i)
            IE_refcat <- simple_emp_int(data = cbind(RIvals[[ref_cat]], EmpDat[, torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)

            for (cat in nonref_cats) {
              result[cat, ] <- simple_emp_int(data = cbind(RIvals[[cat]], EmpDat[, torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point) - IE_refcat
            }
          }
        } else { # now for assumption "A.II''"
          for (i in seq_along(length(RIvals_prep))) {
            run_in_parent(prepping_for_catRI, i = i)
            all_cats <- c(ref_cat, nonref_cats)
            IE_refcat <- simple_emp_int(data = cbind(RIvals[[ref_cat]], EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0), torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)

            for (cat in nonref_cats) {
              other_cats <- all_cats[all_cats != cat]
              result[cat, ] <- simple_emp_int(data = cbind(RIvals[[cat]], EmpDat[which(rowSums(EmpDat[other_cats]) == 0), torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point) - IE_refcat
            }
          }
        }
      }
      if (continue_mixed) {
        result <- NULL # TOFIX
      }
      return(result)
    }
    if (distribution == "other_standard_opts") {
      reticulate::source_python(system.file("python_scripts", "ProbInt_LinPred.py", package = "CompInt"))

      run_in_parent(int_for_RIunif_empirical)

      progressr::with_progress({
        #p <- progressr::progressor(along = lapply(seq_len(nrow(coef_draws)), function(x) coef_draws[x, ]))
        result <- apply(coef_draws, 1, function(x) {
          #p(sprintf("x=%g", x))
          integrate_LPmods(ints = ints, data = data, LinPred = gsub_complex("[l]", linear_predictor$non_vectorized), thetas = c(0, x), grad_variable = reg_of_interest, fun = inverse_link)
        })
      })
      return(result)
    }
  }
}
