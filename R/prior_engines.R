#' CAR operation
#'
#' @param W matrix, neighborhood matrix.
#' @param rho numeric, default = 0.95, car parameter.
#'
#' @return covariance matrix.
car = function(W, rho = .95) {
  solve(diag(rowSums(W)) - rho * W)
}


#' prior engine: CAR conditional for alpha covariance in models with more than one group
#'
#' @param proj `fastan::project` object.
#' @param neib function that calculates the neighborhood matrix.
#' @param tau numeric, to scale CAR matrix.
#'
#' @return list of covariance matrices.
#'
#' @export
car_conditional = function(proj, neib, tau) {
  B = tau * car(neib(proj$space))
  cov =
    1:n.fac(proj) |>
    lapply(function(x) matrix(0, proj$data$dim$row, proj$data$dim$row))
  lim = proj$data$dim$group.sizes |> group_limits()
  for (k in 1:n.fac(proj)) {
    rows = lim[[1]][k]:lim[[2]][k]
    if (proj$prior$semi.conf) {
      rows_extra = lim[[1]][n.fac(proj)+1]:lim[[2]][n.fac(proj)+1]
      rows = c(rows, rows_extra)
    }

    B11 = B[rows,rows]
    B12 = B[rows,-rows]
    B21 = B[-rows,rows]
    B22 = B[-rows,-rows]
    cov[[k]][rows,rows] = B11 - B12 %*% solve(B22) %*% B21
  }

  list(cov = cov, omit.alpha0 = T)
}


#' prior engine: CAR with simple neighborhood matrix
#'
#' (used only for time dependency on lambdas)
#'
#' @inheritParams car_conditional
#'
#' @return list of covariance matrices.
#'
#' @export
car_simple = function(proj, tau) {
  list(cov = lapply(1:n.fac(proj), function(x) tau * car(neib_simple(proj$data$dim$col))))
}


#' prior engine: CAR for alpha exploratory model
#'
#' @inheritParams car_conditional
#'
#' @return list of one covariance matrix.
#'
#' @export
car_expl = function(proj, neib, tau) {
  list(cov = list(tau * car(neib(proj$space))))
}
