#' Title
#'
#' @param data
#' @param chains
#' @param param
#'
#' @return
#'
#' @examples
#' param must be a list with alpha, lambda, sigma2 (or pred)
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

  return( init )
}


#' Title
#'
#' @param model .
#'
#' @return
#'
#' @export
adjust_data_interface = function(model) {
  obs = model$data_fa |>
    dplyr::select(all_of(c("value", "row", "col", "group"))) |>
    as.matrix() |>
    `colnames<-`(NULL)

  list(
    obs_arg         = obs[,1],
    obs_coor        = obs[,2:4],
    al_row          = model$dim$al_row,
    al_col          = model$dim$al_col,
    al_fac          = model$dim$al_fac,
    obs_row         = model$dim$obs_row,
    obs_col         = model$dim$obs_col,
    var_alpha_prior = model$var_alpha_prior,
    sentinel        = model$sentinel
  )
}


#' Title
#'
#' @param data .
#' @param path_interface .
#' @param path_dump .
#' @param init .
#' @param iter .
#' @param warmup .
#' @param chains .
#' @param thin .
#' @param save.data .
#'
#' @return
#'
#' @examples
#'
#' @export
#'
#' @import rstan
run_stan = function(model, init = NULL, iter, warmup = NULL, chains = 1, ...) {

  rstan::stan(file   = file.path("inst/stan/interface_fa_sc.stan"),
              data   = adjust_data_interface(model),
              iter   = iter,
              warmup = ifelse(!is.null(warmup), warmup, floor(iter / 2)),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = ifelse(!is.null(init)  , init  , fiat_init(model = model, chains = chains)),
              chains = chains,
              ...
              )

}



#' Title
#'
#' @param project_folder .
#' @param param .
#'
#' @return
#'
#' @examples
#'
#' @export
get_chains_mcmc = function(project_folder, param) {
  output = get_rdata(file.path(project_folder, "output.Rdata"))

  draws = posterior::as_draws(output)
  chains = list()
  for (i in 1:length(output@inits)) {
    chains[[i]] = posterior::subset_draws(draws, chain=i, variable=param) |> c() |> coda::mcmc()
  }

  coda::as.mcmc.list(chains)
}


#' Title
#'
#' @param samp .
#' @param data .
#'
#' @return
#'
#' @examples
#'
#' @export
#'
#' @import abind
#' @import coda
summary_matrix = function(samp, data = NULL) {

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

    if (!is.null(data) & !is.null(data$real)) {
      matrices[[parameter]][["real"]] = data[["real"]][[parameter]]
    }
  }

  sapply(matrices, function(x) { abind::abind(x, along = 3) })
}


#' Title
#'
#' @param samp .
#' @param row .
#'
#' @return
#'
#' @examples
#'
#' @export
invert_lambda_signal = function(samp, row) {

}
