#' Title
#'
#' @param data .
#' @param dependence .
#' @param semi.conf .
#'
#' @export
prior_normal = function(data, dependence = list(lambda = F, alpha = F), semi.conf) {
  nfac = data$dim$group.n - as.integer(semi.conf)
  prior = list(alpha  = list(mean = list_vec(data$dim$row, nfac)),
               lambda = list(mean = list_vec(data$dim$col, nfac)),
               sigma2 = list(shape=0.1, scale = 1),
               semi.conf = semi.conf,
               type = "normal"
               )

  if (dependence$lambda) {
    lambda_cov = lambda_cov_dep(data$dim$col)
  } else {
    lambda_cov = lambda_cov_indep(data$dim$col)
  }

  alpha_var = alpha_var(data$dim$group.sizes, semi.conf)
  for (i in 1:nfac) {
    prior[["lambda"]][["cov"]][[i]] = lambda_cov
    prior[["alpha"]][["cov"]][[i]] = diag(alpha_var[,i])
  }

  class(prior) = "prior"
  prior
}


#' Adjust the data argument on `rstan::stan()`
#'
#' @param proj fastan proj object.
#'
#' @import abind
interface_normal = function(proj) {
  list(
    sigma2_shape = proj$prior$sigma2$shape,
    sigma2_scale = proj$prior$sigma2$scale,
    alpha_mean   = abind::abind(proj$prior$alpha$mean,  along=2) |> aperm(c(2,1)),
    alpha_cov    = abind::abind(proj$prior$alpha$cov,   along=3) |> aperm(c(3,1,2)),
    lambda_mean  = abind::abind(proj$prior$lambda$mean, along=2) |> aperm(c(2,1)),
    lambda_cov   = abind::abind(proj$prior$lambda$cov,  along=3) |> aperm(c(3,1,2))
  )
}


#' Title
#'
#' @param col .
#' @param rho .
lambda_cov_dep = function(col, rho = .95) {
  D =
    rep(2, col-2) |>
    {\(.) c(1, ., 1)}() |>
    diag()

  W = matrix(0, nrow = col, ncol = col)
  W[1,2] = 1
  W[col,col-1] = 1
  for (i in 2:(col-1)) {
    W[i, c(i-1, i+1)] = 1
  }

  solve(D - rho * W)
}


#' Title
#'
#' @param col .
lambda_cov_indep = function(col) {
  diag(rep(1, col))
}


#' Title
#'
#' @param vec.len .
#' @param list.len .
#' @param mean .
list_vec = function(vec.len, list.len, mean = 0) {
  l = list()
  for (i in 1:list.len) {
    l[[i]] = rep(mean, vec.len)
  }
  l
  # matrix(mean, nrow = sum(group.sizes), ncol = length(group.sizes) - as.numeric(semi.conf))
}


#' Title
#'
#' @param group.sizes .
#' @param semi.conf .
alpha_var = function(group.sizes, semi.conf) {
  alpha_v = matrix(1e-2, nrow = sum(group.sizes), ncol = length(group.sizes) - as.numeric(semi.conf))
  limits = fiat_groups_limits(group.sizes)

  for (i in 1:(length(group.sizes) - as.numeric(semi.conf))) {
    alpha_v[limits[[1]][i]:limits[[2]][i], i] = 10
  }

  if (semi.conf) {
    i = i + 1
    alpha_v[limits[[1]][i]:limits[[2]][i],] = 10
  }

  alpha_v
}


#' Title
#'
#' @param prior .
alpha_cov_to_var = function(prior) {
  lapply(prior$alpha$cov, {\(.) diag(.) |> as.matrix()}) |>
    {\(.) do.call(cbind, .)}()
}
