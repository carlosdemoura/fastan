#' FALTA PREDICTION
#'
#' Generate initial values for STAN MCMC
#'
#' @param model fastanModel object.
#' @param chains integer; number of chains.
#' @param param numeric or list; if it's a list it must contain alpha, lambda, sigma2, and (if predictions are being made) pred; the DEFAULT is `1`.
#'
#' @return list; for each
#'
#' @export
fiat_init = function(model, chains, param = 1) {
  if ( (length(param) == 1) & is.numeric(param) ) {
    x = param
    param = list()
    for (p in c("alpha", "lambda", "sigma2", "pred")) {
      param[[p]] = rep(x, chains)
    }
  }

  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(param$alpha[i] , nrow = model$dim$al_row,  ncol = model$dim$al_fac),
                     lambda = matrix(param$lambda[i], nrow = model$dim$al_fac,  ncol = model$dim$al_col),
                     sigma2 = matrix(param$sigma2[i], nrow = model$dim$al_row,  ncol = 1)
                     #,
                     #pred   = matrix(param$pred[i]  , nrow = model$dim$obs_row, ncol = model$dim$obs_col)
    )

    if(is.null(T)) {
      init[[1]]$pred = matrix(as.numeric(pred), nrow = data$obs_row, ncol = data$obs_col)
    }
  }

  return(init)
}


#' Adjust the data argument on `rstan::stan()`
#'
#' @param model fastan model object.
#'
#' @return list; goes on the data argument in `rstan::stan()`.
#'
#' @export
#'
#' @import purrr
adjust_data_interface = function(model) {
  list(
    obs_arg         = model$data[,1] |> as.vector() |> unname() |> purrr::pluck(1),
    obs_coor        = model$data[,2:3] |> as.matrix() |> unname(),
    al_row          = model$dim$al_row,
    al_col          = model$dim$al_col,
    al_fac          = model$dim$al_fac,
    obs_row         = model$dim$obs_row,
    obs_col         = model$dim$obs_col,
    var_alpha_prior = model$var_alpha_prior,
    sentinel        = model$sentinel
  )
}


#' Run STAN MCMC
#'
#' @param model .
#' @param init .
#' @param chains .
#' @param ... arguments that will be passed to `rstan::stan()`
#'
#' @return rstan::stan fit object.
#'
#' @export
#'
#' @import rstan
run_stan = function(model, init = NULL, chains = 1, pred = F, ...) {
  if (is.null(init)) {
    init = fiat_init(model, chains, 1)
  }
  rstan::stan(file   = file.path("inst/stan/interface_fa_sc.stan"),
              data   = adjust_data_interface(model),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
              )
}


#' Transform `rstan` fit into `coda::mcmc.list()`
#'
#' @param fit `rstan::stan()` object.
#' @param param string; must be in rstan::stan fit format.
#'
#' @return `coda::mcmc.list()`
#'
#' @export
get_chains_mcmc = function(fit, param) {
#get_chains_mcmc = function(project_folder, param) {
  draws = posterior::as_draws(fit)
  chains = list()
  for (i in 1:length(fit@inits)) {
    chains[[i]] = posterior::subset_draws(draws, chain=i, variable=param) |> c() |> coda::mcmc()
  }
  coda::as.mcmc.list(chains)
}


#' Get summary from `rstan::stan()` fit object
#'
#' @param fit `rstan::stan()` fit object.
#' @param model fastan model object
#'
#' @return list of matrices.
#'
#' @export
#'
#' @import abind
#' @import coda
#' @import rstan
summary_matrix = function(fit, model = NULL) {
  samp = rstan::extract(fit)
  matrices = list()

  for (parameter in setdiff(names(samp), "lp__") ) {

    hpd_temp = apply( samp[parameter][[1]], c(2,3),
                      function(x) {
                        coda::HPDinterval(coda::as.mcmc(x))[,c("lower", "upper")] |>
                          as.numeric() |>
                          list()
                      })

    matrices[[parameter]] = list(
      "mean"    = apply( samp[parameter][[1]], c(2,3), mean   )                          ,
      "median"  = apply( samp[parameter][[1]], c(2,3), median )                          ,
      "sd"      = apply( samp[parameter][[1]], c(2,3), sd     )                          ,
      "hpd_min" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(1) }) ,
      "hpd_max" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(2) }) ,
      "hpd_amp" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> diff() })
    )

    if (parameter == "alpha") {
      matrices[[parameter]][["hpd_contains_0"]] = ifelse((matrices[[parameter]][["hpd_max"]] >= 0) & (matrices[[parameter]][["hpd_min"]] <= 0), T, F)
    }

    if (!is.null(model) & !is.null(model$real)) {
      matrices[[parameter]][["real"]] = model[["real"]][[parameter]]
    }
  }

  sapply(matrices, function(x) { abind::abind(x, along = 3) })
}


#' Invert the signal of a column of lambda on the MCMC
invert_lambda_signal = function() {

}


#' Get diagnostic statistics in a data.frame
#'
#' @import dplyr
#' @import tidyr
diagnostic_statistics = function(fit) {
  df =
    summary(fit)$summary |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., par = row.names(.))}() |>
    dplyr::select(dplyr::all_of(c("n_eff", "Rhat", "par"))) |>
    dplyr::rename(neff = "n_eff", rhat = "Rhat") |>
    tidyr::extract(par, into = c("par", "row", "col"), regex = "([a-zA-Z0-9_]+)\\[(\\d+),(\\d+)\\]", convert = TRUE)
  df[nrow(df),] = c(df[nrow(df),1:2], "lp__", 1, 1)
  df
}
