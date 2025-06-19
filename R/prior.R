prior_sc = function(data, dependence.lambda = F, dependence.alpha = F, semi.conf) {
  prior = list(lambda = list(mean = lambda_mean(data$dim$col)),
               alpha  = list(mean = alpha_mean(data$dim$group.sizes, semi.conf)),
               sigma2 = list(shape=.01, scale = .01),
               semi.conf = semi.conf
               )

  if (dependence.lambda) {
    prior[["lambda"]][["cov"]] = lambda_cov_dep(data$dim$col)
  } else {
    prior[["lambda"]][["cov"]] = lambda_cov_indep(data$dim$col)
  }

  prior[["alpha"]][["var"]] = alpha_var(data$dim$group.sizes, semi.conf)

  prior
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
#' @param col .
lambda_mean = function(col, mean = 0) {
  rep(mean, col)
}


#' Title
#'
#' @param group.sizes .
#' @param semi.conf .
alpha_mean = function(group.sizes, semi.conf, mean = 0) {
  matrix(mean, nrow = sum(group.sizes), ncol = length(group.sizes) - as.numeric(semi.conf))
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
