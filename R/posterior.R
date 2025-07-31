#' Generate constant initial values for STAN MCMC
#'
#' @param proj `fastan::project` object.
#' @param chains integer, number of chains.
#'
#' @return list compatible with STAN init.
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


#' Adjust the data argument on `rstan::stan`
#'
#' @param proj `fastan::proj` object.
#'
#' @return list that goes on the data argument in `rstan::stan`.
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
    obs          = proj$data$obs[,1] |> as.vector() |> unname() |> purrr::pluck(1),
    obs_coor     = proj$data$obs[,2:3] |> as.matrix() |> unname()
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
    data_norm = interface_normal(proj)
  } else {
    stop("prior type not accepted")
  }

  data_alpha = list(
    omit_alpha0    = as.numeric(proj$prior$alpha$omit.alpha0),
    n_groups       = length(proj$data$dim$group.sizes),
    group_lim      = group_limits(proj$data$dim$group.sizes) |> {\(.) do.call(cbind, .)}(),
    semi_conf      = as.numeric(proj$prior$semi.conf),
    alpha_in_group = proj$prior$alpha$in_group |> {\(.) if (proj$prior$alpha$omit.alpha0) . else . + 1}() |> {\(.) {.[.>0] = 1:length(.[.>0]); .}}()
  )

  c(data, data_pred, data_norm, data_alpha)
}


#' Run STAN MCMC
#'
#' @param proj `fastan::project` object.
#' @param init initial value, default = NULL, if NULL `fastan::init` is called.
#' @param chains integer, default = 1, number of MCMC chains.
#' @param pred logical, default = `TRUE`, `TRUE` if the predictions should be made, `FALSE` if otherwise.
#' @param ... arguments that will be passed to `rstan::stan`.
#'
#' @return `rstan::stanfit` object.
#'
#' @export
#'
#' @importFrom rstan stan
stan = function(proj, init = NULL, chains = 1, pred = T, ...) {
  if (is.null(init)) {
    init = init(proj, chains)
  }

  pars = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}()
  data = interface(proj)
  if (!pred & !is.null(proj$data$pred)) {
    data[c("pred_coor", "n_pred")] = list(t(as.matrix(1:2)), 0)
    pars = setdiff(pars, "pred")
  }

  type = proj$prior$type
  if (type == "normal") {
    file = system.file("stan", "model.stan", package = "fastan")
  } else {
    stop("prior type not accepted")
  }

  rstan::stan(file   = file,
              data   = data,
              pars   = pars,
              init   = init,
              chains = chains,
              ...
  )
}


#' Get summary from `rstan::stanfit` object
#'
#' @param proj `fastan::project` object.
#' @param bias.stat string, default = "mean", Bayes estimator to be used to calculate bias if data is simdata.
#' @param samp list, if its's NULL (default), then the sample is extracted from `proj$fit`, if not, pass the sample here.
#'
#' @return list of matrices.
#'
#' @export
#'
#' @import abind
#' @importFrom coda as.mcmc HPDinterval
#' @import purrr
#' @importFrom rstan extract
#' @importFrom stats density median sd
summary_matrix = function(proj, bias.stat = "mean", samp = NULL) {
  get_mode = function(x) {
    d = stats::density(x)
    d$x[which.max(d$y)]
  }

  fit = proj$fit
  data = proj$data
  if (is.null(samp)) {
    samp = rstan::extract(fit)
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
      "mean"    = apply( samp[parameter][[1]], c(2,3), mean          )                   ,
      "median"  = apply( samp[parameter][[1]], c(2,3), stats::median )                   ,
      "mode"    = apply( samp[parameter][[1]], c(2,3), get_mode      )                   ,
      "sd"      = apply( samp[parameter][[1]], c(2,3), stats::sd     )                   ,
      "hpd_min" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(1) }) ,
      "hpd_max" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(2) }) ,
      "hpd_amp" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> diff() })
    )

    if (parameter == "alpha") {
      matrices[[parameter]][["hpd_contains_0"]] = ifelse((matrices[[parameter]][["hpd_max"]] >= 0) & (matrices[[parameter]][["hpd_min"]] <= 0), T, F)
      matrices[[parameter]][["in_group"]] = alpha_in_group(data$dim$group.sizes, proj$prior$semi.conf)
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


#' Get diagnostic statistics from `rstan::stanfit`
#'
#' @param fit `rstan::stanfit` object.
#'
#' @return `tibble::tibble` containt diagnostic statistics.
#'
#' @export
#'
#' @import dplyr
#' @importFrom coda geweke.diag
#' @import purrr
#' @importFrom rstan extract
#' @importFrom tidyr separate
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

  summary(fit)$summary |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., par = row.names(.))}() |>
    dplyr::select(dplyr::all_of(c("n_eff", "Rhat", "par"))) |>
    merge(get_geweke(fit), by = "par") |>
    {\(.) {.[.$par=="lp__","par"] = "lp__[1,1]"; .}}() |>
    tidyr::separate(.data$par, into = c("par", "index"), sep = "\\[") |>
    tidyr::separate(.data$index, into = c("row", "col"), sep = ",") |>
    dplyr::mutate(
      col = as.numeric(gsub("]", "", .data$col)),
      row = as.numeric(.data$row)
    ) |>
    dplyr::as_tibble()
}


#' Transform `rstan::stanfit` into `coda::mcmc.list`
#'
#' @param fit `rstan::stanfit` object.
#' @param param string, must be in the `rstan::stanfit` format.
#'
#' @return `coda::mcmc.list` object.
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


#' Transform summary from list of 3D matrices to list of tibbles
#'
#' @param proj `fastan::project` object
#' @param par string vector, parameters to be returnd, default = NULL returns all parameters.
#'
#' @return list of `tibble:tibble`.
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
summary_as_df = function(proj, par = NULL) {
  as_df = function(par) {
    df =
      proj$summary[[par]] |>
      {\(x)
        do.call(rbind, lapply(dimnames(x)[[3]], function(slice) {
          x[, , slice, drop = FALSE] |>
            as.data.frame() |>
            {\(.) `colnames<-`(., 1:ncol(.)) }() |>
            {\(.) dplyr::mutate(., row = row.names(.)) }() |>
            {\(.) tidyr::pivot_longer(., names_to = "col", values_to = "v", cols = 1:ncol(x)) }() |>
            {\(.) dplyr::mutate(., stat = slice) }()
        }))
      }() |>
      tidyr::pivot_wider(values_from = "v", names_from = "stat") |>
      {\(.) dplyr::mutate(
        .,
        row = as.numeric(.$row),
        col = as.numeric(.$col)
      )}()

    if (!is.null(par) & !is.null(proj$data) & (par != "pred")) {
      if (par %in% c("alpha", "sigma2")) {
        df_ = data.frame(id = proj$data$label$loading)
        df_$row = 1:nrow(df_)
      } else if (par == "lambda") {
        df_ = data.frame(id = proj$data$label$factor_level)
        df_$col = 1:nrow(df_)
      } else {
        stop("label must be either a parameter or NULL")
      }

      df = dplyr::left_join(df, df_)
    }

    df
  }

  if (is.null(par)) {
    par =
      c("alpha", "lambda", "sigma2") |>
      {\(.) if (is.null(proj$data$pred)) . else c("pred")}()
  }

  ans = lapply(par, as_df)
  names(ans) = par

  ans
}
