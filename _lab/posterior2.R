
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

stan2 = function(proj, init = NULL, alpha_nullify = T, chains = 1, ...) {
  if (is.null(init)) {
    init = init(proj, chains)
  }

  type = proj$prior$type
  if (type == "normal") {
    file = "D:/carlos/01_pesquisa/fastan/_lab/interface_fa_normal2.stan"
  } else {
    stop("prior type not accepted")
  }

  data = interface(proj)
  data[["alpha_nullify"]] = as.logical(alpha_nullify)

  rstan::stan(file   = file,
              data   = data,
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(proj$data$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
              )
}

