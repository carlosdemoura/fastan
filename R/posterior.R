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
#' @param pred .
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
  if (class(fit) == "stanfit") {
    samp = rstan::extract(fit)
  } else {
    samp = fit |>
      lapply(function(x) {dim(x) = c(prod(utils::head(dim(x), -2)), utils::tail(dim(x), 2)); x})
  }

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


#' Get diagnostic statistics from fit
#'
#' @import dplyr
#' @import tidyr
diagnostic_fit = function(fit) {
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


#' Get diagnostic statistics from draws
#'
#' @import dplyr
#' @import tidyr
diagnostic_draws = function(draws) {
  draws
}


#' Invert the signal of a column of lambda on the MCMC
invert_lambda_signal = function() {

}


#' Generate initial values for STAN MCMC from another fit
#'
#' @param draws fastanModel object.
#'
#' @return list; for each
#'
#' @export
fiat_init_from_draws = function(draws) {
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

  init = list()
  for (chain in 1:dim(draws$alpha)[2]) {
    init[[chain]] = list()
    for (par in c("alpha", "lambda", "sigma2")) {
      init[[chain]][[par]] =
        draws[[par]] |>
        {\(.) .[dim(.)[1],chain,,] }() |>
        correct_dimensions(par)
    }
  }
  init
}

#' Generate initial values for STAN MCMC from another fit
#'
#' @param draws1,draws2 draws
#'
#' @export
#'
#' @import abind
merge_draws = function(draws1, draws2) {
  x = sapply(
    list("alpha", "lambda", "sigma2"),
    function(x) {
      abind::abind(draws1[[x]], draws2[[x]], along = 1)
    })
  names(x) = c("alpha", "lambda", "sigma2")
  x
}
