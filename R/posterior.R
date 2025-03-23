#' FALTA PREDICTION
#'
#' Generate initial values for STAN MCMC
#'
#' @param model fastanModel object.
#' @param chains integer; number of chains.
#'
#' @return list; for each
#'
#' @export
fiat_init = function(model, chains) {
  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(1, nrow = model$dim$al_row,  ncol = model$dim$al_fac),
                     lambda = matrix(0, nrow = model$dim$al_fac,  ncol = model$dim$al_col),
                     sigma2 = matrix(1, nrow = model$dim$al_row,  ncol = 1)
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
    init = fiat_init(model, chains)
  }
  rstan::stan(file   = file.path("inst/stan/interface_fa_sc.stan"),
              data   = adjust_data_interface(model),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
              )
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


#' Invert the signal of a column of lambda on the MCMC
invert_lambda_signal = function() {

}


#' Transform `rstan` fit into `coda::mcmc.list()`
#'
#' @param fit `rstan::stan()` object.
#' @param param string; must be in rstan::stan fit format.
#'
#' @return `coda::mcmc.list()`
#'
#' @export
#'
#' @import posterior
get_chains_mcmc = function(fit, param) {
  draws = posterior::as_draws(fit)
  chains = list()
  for (i in 1:length(fit@inits)) {
    chains[[i]] = posterior::subset_draws(draws, chain=i, variable=param) |> c() |> coda::mcmc()
  }
  coda::as.mcmc.list(chains)
}


#' Generate initial values for STAN MCMC from another fit
#'
#' @param fit fastanModel object.
get_all_chains = function(fit) {
  params = fit@sim$pars_oi |> utils::head(-1)
  chains = fit@sim$chains
  dims   = fit@sim$dims_oi|> utils::head(-1)
  iter   = fit@sim$iter

  get_draws_per_parameter = function(param) {
    dim = dims[[param]]
    answer = array(dim = c(dim, iter, chains))  # dimnames = c("row", "col", "iter", "chain")
    for (row in 1:dim[1]) {
      for (col in 1:dim[2]) {
        answer[row, col, , ] =
          get_chains_mcmc( fit, param |> paste0("[", row, ",", col, "]") ) |>
          sapply(function(x){ x |> c()})
      }
    }
    answer
  }

  all_chains = list()
  for (param in params) {
    all_chains[[param]] = get_draws_per_parameter(param)
  }
  all_chains
}


#' Adjust the data argument on `rstan::stan()`
#'
merge_chains = function(chain1, chain2) {
  answer = list()
  for (par in c("alpha", "lambda", "sigma2")) {
    chains = dim(chain1[[par]])[4]
    iter1  = dim(chain1[[par]])[3]
    iter2  = dim(chain2[[par]])[3]

    new_chain = array(dim = c(dim(chain1[[par]])[1:2], iter1 + iter2, chains))
    new_chain[,,1:iter1,] =
      chain1[[par]]
    new_chain[,,(iter1+1):(iter1+iter2),] =
      chain2[[par]]
    answer[[par]] = new_chain
  }
  answer
}


#' Generate initial values for STAN MCMC from another fit
#'
#' @param fit fastanModel object.
#'
#' @return list; for each
#'
#' @export
fiat_init_from_fit = function(draws) {
  chains = dim(draws$alpha)[4]
  x =
    sapply(
      draws,
      function(x) { x[,,dim(x)[3],, drop = FALSE]
      })

  correct_dimensions = function(x, par) {
    if(is.null(dim(x))) {
      if (par == "lambda"){
        answer = matrix(x, nrow = 1)
      } else {
        answer = matrix(x, ncol = 1)
      }
    } else {
      answer = x
    }
    return(answer)
  }

  init_temp = list()
  for (par in c("alpha", "lambda", "sigma2")) {
    init_temp[[par]] =
      lapply(
        1:dim(x[[par]])[4],
        function(i) {
          x[[par]][,,,i] |> correct_dimensions(par)

        })
  }

  init = vector("list", length = chains)
  for (chain in 1:chains) {
    init[[chain]] = lapply(init_temp, function(x) x[[chain]])
    names(init[[chain]]) = names(init_temp)
  }

  init
}
