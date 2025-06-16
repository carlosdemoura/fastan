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
                     )

    if(!is.null(model$pred)) {
      init[[i]]$pred = matrix(0, nrow = nrow(model$pred), ncol = 1)
    }
  }

  return(init)
}


#' Generate initial values for STAN MCMC from another fit
#'
#' @param fit stanfit object.
#'
#' @return list; for each
#'
#' @export
fiat_init_from_last_value = function(fit) {
  draws = my_extract(fit)
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


#' Adjust the data argument on `rstan::stan()`
#'
#' @param model fastan model object.
#'
#' @return list; goes on the data argument in `rstan::stan()`.
#'
#' @export
#'
#' @import purrr
#' @import dplyr
adjust_data_interface = function(model) {
  data = list(
    obs             = model$data[,1] |> as.vector() |> unname() |> purrr::pluck(1),
    obs_coor        = model$data[,2:3] |> as.matrix() |> unname(),
    al_row          = model$dim$al_row,
    al_col          = model$dim$al_col,
    al_fac          = model$dim$al_fac,
    var_alpha_prior = model$var_alpha_prior
    ) |>
    {\(.) c(., list(obs_n = length(.$obs)))}()

  if (is.null(model$pred)) {
    data_pred = list(
      pred_coor     = matrix(1:2, nrow = 1),
      pred_n        = 0
      )
  } else {
    data_pred = list(
      pred_coor     = model$pred[,2:3] |> as.matrix() |> unname()
      ) |>
      {\(.) c(., list(pred_n = nrow(.$pred_coor)))}()
  }

  return(c(data, data_pred))
}


#' Run STAN MCMC
#'
#' @param model .
#' @param init .
#' @param chains .
#' @param ... arguments that will be passed to `rstan::stan()`
#'
#' @return rstan::stanfit object.
#'
#' @export
#'
#' @import rstan
stan = function(model, init = NULL, chains = 1, ...) {
  if (is.null(init)) {
    init = fiat_init(model, chains)
  }
  rstan::stan(file   = system.file("stan", "interface_fa_sc.stan", package = "fastan"),
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
#' @import stats
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
      "mean"    = apply( samp[parameter][[1]], c(2,3), mean          )                   ,
      "median"  = apply( samp[parameter][[1]], c(2,3), stats::median )                   ,
      "sd"      = apply( samp[parameter][[1]], c(2,3), stats::sd     )                   ,
      "hpd_min" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(1) }) ,
      "hpd_max" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(2) }) ,
      "hpd_amp" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> diff() })
    )

    if (parameter == "alpha") {
      matrices[[parameter]][["hpd_contains_0"]] = ifelse((matrices[[parameter]][["hpd_max"]] >= 0) & (matrices[[parameter]][["hpd_min"]] <= 0), T, F)
    }

    if ((parameter == "pred") & !is.null(model)) {
      matrices[[parameter]][["row_"]] = model$pred$row |> as.matrix()
      matrices[[parameter]][["col_"]] = model$pred$col |> as.matrix()
    }

    if (!is.null(model) & !is.null(model$real) & (parameter != "pred")) {
      matrices[[parameter]][["real"]] = model[["real"]][[parameter]]
    }
  }

  sapply(matrices, function(x) { abind::abind(x, along = 3) })
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
#' @import coda
#' @import posterior
get_chains_mcmc = function(fit, param) {
  #get_chains_mcmc = function(project_folder, param) {
  draws = posterior::as_draws(fit)
  chains = list()
  for (i in 1:length(fit@inits)) {
    chains[[i]] = posterior::subset_draws(draws, chain=i, variable=param) |> c() |> coda::mcmc()
  }
  coda::as.mcmc.list(chains)
}


#' Get diagnostic statistics from fit
#'
#' @param fit stanfit object
#'
#' @export
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


#' Get percentage of parameters that are in HPD from simdata
#'
#' @param smry fastan summary
#'
#' @export
#'
#' @import stats
percentage_hits = function(smry) {
  table =
    matrix(0, nrow = 4, ncol = 2) |>
    as.data.frame() |>
    `colnames<-`(c("p", "total")) |>
    `rownames<-`(c("alpha", "lambda", "sigma2", "all"))
  for (par in c("alpha", "lambda", "sigma2")) {
    x =
      smry[[par]][,,"real"] |>
      {\(.) (. >= smry[[par]][,,"hpd_min"]) & (. <= smry[[par]][,,"hpd_max"])}() |>
      as.numeric()
    table[par, ] = c(mean(x), length(x))
  }
  table["all",] = stats::weighted.mean(table$p, table$total) |> c(sum(table$total))
  table
}

#' Get gewekes
#'
#' @param fit stan fit
#' @param par parameter
#'
#' @import rstan
#' @import coda
#' @import purrr
#'
#' @export
get_geweke = function(fit, par = "all") {
  fit |>
    {\(.) if (par != "all") rstan::extract(., permuted = F, par = par)
      else rstan::extract(., permuted = F)}() |>
    apply(c(2, 3), function(x) {coda::geweke.diag(x) |> purrr::pluck(1) |> unname()} ) |>
    as.vector()
}


#' Adjust summary for real values
#'
#' @param smry fastan summary
#'
#' @export
adjust_summary = function(smry) {
  stats = c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")
  smry2 = smry
  for (factor in 1:dim(smry$lambda)[1]) {

    c = numeric(dim(smry$lambda)[2])
    p = numeric(dim(smry$lambda)[2])

    for (col in 1:dim(smry$lambda)[2]) {
      c[col] = (smry$lambda[factor,col,"mean"] / smry$lambda[factor,col,"real"]) |> unname()
      smry2$lambda[factor,,stats] = (1/c[col]) * smry$lambda[factor,,stats, drop = F]
      smry2$alpha[factor,,stats]  = c[col]     * smry$alpha[factor,,stats, drop = F]
      p[col] = percentage_hits(smry2)["all", "p"]
    }

    c = c[which(p == max(p))[1]]
    smry$lambda[factor,,stats] = (1/c) * smry$lambda[factor,,stats, drop = F]
    smry$alpha[factor,,stats]  = c     * smry$alpha[factor,,stats, drop = F]

  }
  smry
}
