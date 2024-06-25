#' @export
get_IE <- function(model_fit, reg_of_interest = NULL, integration = NULL, seed = NULL, ndraws = 10000, catRIbin = FALSE, ...) {
  separate_interactions <- FALSE
  run_in_parent(getting_situated1)
  integration(model = model)
  run_in_parent(getting_situated2)

  if (!(any(reg_of_interest %in% regs(model)) || is.null(reg_of_interest))) {
    stop("'reg_of_interest' must either be a variable used for the model specification or set to NULL.")
  }
  if (is.null(reg_of_interest) && any(assumption %in% c("A.II'"))) {
    stop("In order to calculate individualized expectations under assumptions A.II', a regressor of interest must be defined using the variable reg_of_interest.")
  }

  if (model[["type"]] %in% c("GLM", "GLMM")) {
    coef_draws <- draws_from_paramdist(model = model, ndraws = ndraws, seed = seed, ...)

    linear_predictor <- make_linear_predictor(mod = model, reg_of_interest = reg_of_interest, separate_interactions = FALSE)

    run_in_parent(getting_inverse)


    eval_g_theta_at_point <- eval(parse(text = paste(
      "function(theta,l){",
      make_g_theta(model_type = model[["type"]], linear_predictor = linear_predictor, inverse_link = inverse_link, vectorized = FALSE, ...),
      "}"
    )))

    if (distribution == "empirical") {
      run_in_parent(empirical_Int_catmet_handling)
      run_in_parent(data_asmpt)

      if (continue_metric) {
        result <- simple_emp_int(data = EmpDat, coef_draws = coef_draws, f = eval_g_theta_at_point)
      }

      if (continue_categorical) {
        if ("refcat" %in% ellipsisvars) {
          # TOFIX #Code for when the RI's reference category should be one that is not specified in the model
        }
        RIvals_prep <- dealing_with_catRI(dat = EmpDat, g_theta = eval_g_theta_at_point, RIname = reg_of_interest)
        run_in_parent(prepping_for_catRI, i = 1)

        result <- matrix(nrow = length(nonref_cats) + 1, ncol = ndraws)
        rownames(result) <- c(ref_cat, nonref_cats)

        torem <- setdiff(names(EmpDat), nonref_cats)

        if (assumption %in% c("A.I", "A.II'")) {
          result[ref_cat, ] <- simple_emp_int(data = cbind(RIvals[[ref_cat]], EmpDat[, torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)

          for (cat in nonref_cats) {
            result[cat, ] <- simple_emp_int(data = cbind(RIvals[[cat]], EmpDat[, torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)
          }
        } else { # now for assumption "A.II''"
          all_cats <- c(ref_cat, nonref_cats)
          result[ref_cat, ] <- simple_emp_int(data = cbind(RIvals[[ref_cat]], EmpDat[which(rowSums(EmpDat[nonref_cats]) == 0), torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)

          for (cat in nonref_cats) {
            other_cats <- all_cats[all_cats != cat]
            result[cat, ] <- simple_emp_int(data = cbind(RIvals[[cat]], EmpDat[which(rowSums(EmpDat[other_cats]) == 0), torem, drop = FALSE]), coef_draws = coef_draws, f = eval_g_theta_at_point)
          }
        }
      }

      return(result)
    }
    if (distribution == "other_standard_opts") {
      reticulate::source_python(system.file("python_scripts", "ProbInt_LinPred.py", package = "CompInt"))

      run_in_parent(int_for_RIunif_empirical)

      progressr::with_progress({
        p <- progressr::progressor(along = lapply(seq_len(nrow(coef_draws)), function(x) coef_draws[x, ]))
        result <- apply(coef_draws, 1, function(x) {
          p(sprintf("x=%g", x))
          integrate_LPmods(ints = ints, LinPred = gsub_complex("[l]", linear_predictor$non_vectorized), thetas = c(0, x), data = data, fun = inverse_link)
        })
      })
      return(result)
    }
  }
}
