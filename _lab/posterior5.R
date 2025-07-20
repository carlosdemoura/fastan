
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

stan2 = function(proj, init = NULL, chains = 1, pred = T, ...) {
  if (is.null(init)) {
    init = init(proj, chains)
  }

  pars = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}()
  data = interface2(proj)
  if (!pred & !is.null(proj$data$pred)) {
    data[c("pred_coor", "n_pred")] = list(t(as.matrix(1:2)), 0)
    pars = setdiff(pars, "pred")
  }

  type = proj$prior$type
  if (type == "normal") {
    file = "D:/carlos/01_pesquisa/fastan/_lab/interface_fa_normal5.stan"
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


interface2 = function(proj) {
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
    data_norm = interface_normal(proj)
  } else {
    stop("prior type not accepted")
  }

  data_alpha = list(
    omit_alpha0    = as.numeric(proj$prior$alpha$omit.alpha0),
    n_groups       = length(proj$data$dim$group.sizes),
    group_lim      = fiat_groups_limits(proj$data$dim$group.sizes) |> {\(.) do.call(cbind, .)}(),
    semi_conf      = as.numeric(proj$prior$semi.conf),
    alpha_in_group = proj$prior$alpha$in_group |> {\(.) if (proj$prior$alpha$omit.alpha0) . else . + 1}() |> {\(.) {.[.>0] = 1:length(.[.>0]); .}}()
  )

  c(data, data_pred, data_norm, data_alpha)
}
