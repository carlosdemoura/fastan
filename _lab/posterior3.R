
set_fit2 = function(proj, set_summary = T, set_diagnostic = T, ...) {
  proj$fit = stan2(proj, ...)

  if (set_diagnostic) {
    proj = set_diagnostic(proj)
  }
  if (set_summary) {
    proj = set_summary(proj)
  }
  return(proj)
}

stan2 = function(proj, init = NULL, alpha.nullify = T, chains = 1, ...) {
  if (is.null(init)) {
    init = init(proj, chains)
  }

  type = proj$prior$type
  if (type == "normal") {
    file = "D:/carlos/01_pesquisa/fastan/_lab/interface_fa_normal3.stan"
  } else {
    stop("prior type not accepted")
  }

  data = interface(proj)
  data[["alpha_nullify"]] = as.numeric(alpha.nullify)
  data[["n_groups"]]      = length(proj$data$dim$group.sizes)
  data[["group_size"]]    = proj$data$dim$group.sizes
  data[["group_lim"]]     = fiat_groups_limits(proj$data$dim$group.sizes) |> {\(.) do.call(cbind, .)}()
  data[["semi_conf"]]     = as.numeric(proj$prior$semi.conf)
  if (proj$prior$semi.conf) {
    alpha_vec_lim = data[["group_lim"]]
    alpha_vec_lim = alpha_vec_lim[-nrow(alpha_vec_lim),] + tail(proj$data$dim$group.sizes, 1)
    alpha_vec_lim[1,1] = 1
  } else {
    alpha_vec_lim = data[["group_lim"]]
  }
  data[["n_alpha"]]       = alpha_vec_lim[nrow(alpha_vec_lim),2]

  rstan::stan(file   = file,
              data   = data,
              #pars   = c("alpha", "lambda", "sigma2", "alpha_") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}(),
              #pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}(),
              pars   = c("alpha", "lambda", "sigma2"),
              init   = init,
              chains = chains,
              ...
              )
}


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
