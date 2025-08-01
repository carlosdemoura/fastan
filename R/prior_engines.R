#' CAR operation
#'
#' @param W matrix, neighborhood matrix.
#' @param rho numeric, default = 0.95, car parameter.
#'
#' @return covariance matrix.
car = function(W, rho = .95) {
  solve(diag(rowSums(W)) - rho * W)
}


#' Prior engine for CAR
#'
#' @param proj `fastan::project` object.
#' @param type string, type of CAR, can be either 'conditional', 'expl', or 'simple'.
#' @param neib function that calculates the neighborhood matrix (default NULL, for when type == "siple").
#' @param tau numeric, to scale CAR matrix, default is 1.
#'
#' @return list of covariance matrices.
#'
#' @export
car_engine = function(proj, type, neib = NULL, tau = 1) {
  stopifnot("type must be either 'conditional', 'expl', or 'simple'" = type %in% c("conditional", "expl", "simple"))

  car_conditional = function() {
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

  car_simple = function() {
    list(cov = lapply(1:n.fac(proj), function(x) tau * car(neib_simple(proj$data$dim$col))))
  }

  car_expl = function() {
    list(cov = list(tau * car(neib(proj$space))))
  }

  if (type == "conditional") {
    return(car_conditional())
  } else if (type == "expl") {
    return(car_expl())
  } else if (type == "simple") {
    return(car_simple())
  }

}
