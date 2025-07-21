#' Generate prior with independence between parameters
#'
#' @param data `fastan::data` object.
#' @param semi.conf logical, `TRUE` if model is semi-confirmatory, `FALSE` otherwise.
#' @param omit.alpha0 logical, default = `TRUE`, `TRUE` if you want to force a constant prior for alphas not in group, `FALSE` otherwise.
#' @param ... dot args.
#'
#' @return `fastan::prior` object.
#'
#' @export
prior_normal = function(data, semi.conf, omit.alpha0 = T, ...) {
  list_vec = function(vec.len, list.len, mean = 0) {
    l = list()
    for (i in 1:list.len) {
      l[[i]] = rep(mean, vec.len)
    }
    l
  }

  nfac = length(data$dim$group.sizes) - as.integer(semi.conf)
  prior = list(alpha  = list(mean = list_vec(data$dim$row, nfac)),
               lambda = list(mean = list_vec(data$dim$col, nfac)),
               sigma2 = list(shape = 1, rate = .1, only1 = F),
               semi.conf = semi.conf,
               type = "normal"
               )

  lambda_cov = diag(rep(1, data$dim$col))
  alpha_var =
    alpha_in_group(data$dim$group.sizes, semi.conf) |>
    {\(.) {.[.==0] = 0.1; .[.==1] = 10; .}}()
  for (i in 1:nfac) {
    prior[["lambda"]][["cov"]][[i]] = lambda_cov
    prior[["alpha"]][["cov"]][[i]] = diag(alpha_var[,i])
  }
  prior$alpha[["in_group"]] = alpha_in_group(data$dim$group.sizes, semi.conf)
  prior$alpha[["omit.alpha0"]] = omit.alpha0

  class(prior) = "prior"
  prior
}


#' To be used with `fastan::interface`
#'
#' @param proj `fastan::project` object.
#'
#' @import abind
interface_normal = function(proj) {
  list(
    sigma2_shape = proj$prior$sigma2$shape,
    sigma2_rate  = proj$prior$sigma2$rate,
    sigma2_only1 = as.numeric(proj$prior$sigma2$only1),
    alpha_mean   = abind::abind(proj$prior$alpha$mean,  along=2) |> aperm(c(2,1)),
    alpha_cov    = abind::abind(proj$prior$alpha$cov,   along=3) |> aperm(c(3,1,2)),
    lambda_mean  = abind::abind(proj$prior$lambda$mean, along=2) |> aperm(c(2,1)),
    lambda_cov   = abind::abind(proj$prior$lambda$cov,  along=3) |> aperm(c(3,1,2))
  )
}


#' Create matrix var(alpha) from covariances on prior
#'
#' @param prior .
alpha_cov_to_var = function(prior) {
  lapply(prior$alpha$cov, {\(.) diag(.) |> as.matrix()}) |>
    {\(.) do.call(cbind, .)}()
}
