#' Generate initial values for STAN MCMC
#'
#' @param proj fastan project object.
#' @param chains integer; number of chains.
#'
#' @return list; for each
#'
#' @export
init = function(proj, chains) {
  n.fac = n.fac(proj)
  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(0, nrow = proj$data$dim$row,  ncol = n.fac),
                     lambda = matrix(1, nrow = n.fac            ,  ncol = proj$data$dim$col),
                     sigma2 = matrix(1, nrow = proj$data$dim$row,  ncol = 1)
                     )

    if(!is.null(proj$data$pred)) {
      init[[i]]$pred = matrix(0, nrow = nrow(proj$data$pred), ncol = 1)
    }
  }

  init
}


#' Adjust the data argument on `rstan::stan()`
#'
#' @param proj fastan proj object.
#'
#' @return list that goes on the data argument in `rstan::stan()`.
#'
#' @export
#'
#' @import abind
#' @import purrr
interface = function(proj) {
  data = list(
    n_row        = proj$data$dim$row,
    n_col        = proj$data$dim$col,
    n_fac        = n.fac(proj),
    obs          = proj$data$x[,1] |> as.vector() |> unname() |> purrr::pluck(1),
    obs_coor     = proj$data$x[,2:3] |> as.matrix() |> unname()
  ) |>
    {\(.) c(., list(n_obs = length(.$obs)))}()

  if (is.null(proj$data$pred)) {
    data_pred = list(
      pred_coor     = matrix(1:2, nrow = 1),
      n_pred        = 0
      )
  } else {
    data_pred = list(
      pred_coor     = proj$data$pred[,2:3] |> as.matrix() |> unname()
      ) |>
      {\(.) c(., list(n_pred = nrow(.$pred_coor)))}()
  }

  type = proj$prior$type
  if (type == "normal") {
    x = interface_normal(proj)
  } else {
    stop("prior type not accepted")
  }

  c(data, data_pred, x)
}


#' Run STAN MCMC
#'
#' @param proj .
#' @param init .
#' @param chains .
#' @param ... arguments that will be passed to `rstan::stan()`
#'
#' @return rstan::stanfit object.
#'
#' @export
#'
#' @importFrom rstan stan
stan = function(proj, init = NULL, chains = 1, ...) {
  if (is.null(init)) {
    init = init(proj, chains)
  }

  type = proj$prior$type
  if (type == "normal") {
    file = system.file("stan", "interface_fa_normal.stan", package = "fastan")
  } else {
    stop("prior type not accepted")
  }

  rstan::stan(file   = file,
              data   = interface(proj),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
              )
}


#' Get summary from `rstan::stan()` fit object
#'
#' @param proj .
#' @param bias.stat .
#'
#' @return list of matrices.
#'
#' @export
#'
#' @import abind
#' @importFrom coda as.mcmc HPDinterval
#' @import purrr
#' @importFrom rstan extract
#' @importFrom stats median sd
summary_matrix = function(proj, bias.stat = "mean") {
  fit = proj$fit
  data = proj$data
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

      in_group = real_from_dist(group.sizes = data$dim$group.sizes, columns = data$dim$col, semi.conf = proj$prior$semi.conf)$alpha
      in_group[in_group!=0] = T
      if (proj$prior$semi.conf) {
        x = fiat_groups_limits(data$dim$group.sizes)
        in_group[rev(x[[1]])[1]:rev(x[[2]])[1], ] = 1
      }
      matrices[[parameter]][["in_group"]] = in_group
    }

    if ((parameter == "pred") & !is.null(data)) {
      matrices[[parameter]][["row_"]] = data$pred$row |> as.matrix()
      matrices[[parameter]][["col_"]] = data$pred$col |> as.matrix()
      if (all(!is.na(data$pred$value))) {
        matrices[[parameter]][["real"]] = data$pred$value |> as.matrix()
      }
    }

    if (!is.null(data) & !is.null(data$real) & (parameter != "pred")) {
      matrices[[parameter]][["real"]] = data[["real"]][[parameter]]
    }

    if (!is.null(matrices[[parameter]][["real"]])) {
      denom = matrices[[parameter]][["real"]]
      denom[denom == 0] = 1
      matrices[[parameter]][["bias"]] = (matrices[[parameter]][[bias.stat]] - matrices[[parameter]][["real"]]) / abs(denom)
    }
  }

  matrices = sapply(matrices, function(x) { abind::abind(x, along = 3) })
  class(matrices) = "summary"
  return(matrices)
}


#' Get diagnostic statistics from fit
#'
#' @param fit stanfit object
#'
#' @export
#'
#' @import dplyr
#' @importFrom coda geweke.diag
#' @import purrr
#' @importFrom rstan extract
#' @importFrom tidyr extract
diagnostic = function(fit) {
  fit = validate_proj_arg(fit, "fit")

  get_geweke = function(fit) {
    x =
      fit |>
      rstan::extract(permuted = F) |>
      apply(c(2, 3), function(x) {coda::geweke.diag(x) |> purrr::pluck(1) |> unname()} )

    rbind(colnames(x), unname(x)) |>
      t() |>
      as.data.frame() |>
      `colnames<-`(c("par", paste0("geweke:", 1:nrow(x)))) |>
      dplyr::mutate(dplyr::across(-1, as.numeric))
  }

  df =
    summary(fit)$summary |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., par = row.names(.))}() |>
    dplyr::select(dplyr::all_of(c("n_eff", "Rhat", "par"))) |>
    merge(get_geweke(fit), by = "par") |>
    tidyr::extract("par", into = c("par", "row", "col"), regex = "([a-zA-Z0-9_]+)\\[(\\d+),(\\d+)\\]", convert = TRUE)
  df[is.na(df$par),c("par", "row", "col")] = c("lp__", 1, 1)

  df
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
#' @importFrom coda as.mcmc.list
#' @importFrom posterior as_draws subset_draws
get_chains_mcmc = function(fit, param) {
  #get_chains_mcmc = function(project_folder, param) {
  draws = posterior::as_draws(fit)
  chains = list()
  for (i in 1:length(fit@inits)) {
    chains[[i]] = posterior::subset_draws(draws, chain=i, variable=param) |> c() |> coda::mcmc()
  }
  coda::as.mcmc.list(chains)
}


#' Title
#'
#' @param smry .
#' @param locs .
#'
#' @export
invert_signal_smry = function(smry, locs) {
  if ("bias" %in% dimnames(smry$alpha)[[3]]) {
    stat = c("mean", "median", "bias")
  } else {
    stat = c("mean", "median")
  }
  for (loc in locs) {
    smry$alpha[,loc,c(stat, "hpd_min", "hpd_max")]  = -smry$alpha[,loc,c(stat, "hpd_max", "hpd_min")]
    smry$lambda[loc,,c(stat, "hpd_min", "hpd_max")] = -smry$lambda[loc,,c(stat, "hpd_min", "hpd_max")]
  }
  smry
}
