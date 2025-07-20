#' Title
#'
#' @param W .
#' @param rho .
car = function(W, rho = .95) {
  solve(diag(rowSums(W)) - rho * W)
}


#' Title
#'
#' @param proj .
#' @param neib .
#' @param tau .
#'
#' @export
car_conditional = function(proj, neib, tau) {
  B = tau * car(neib(proj$space))
  cov =
    1:n.fac(proj) |>
    lapply(function(x) matrix(0, proj$data$dim$row, proj$data$dim$row))
  lim = proj$data$dim$group.sizes |> fiat_groups_limits()
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


#' Title
#'
#' @param proj .
#' @param tau .
#'
#' @export
car_simple = function(proj, tau) {
  list(cov = lapply(1:n.fac(proj), function(x) tau * car(neib_simple(proj$data$dim$col))))
}


#' Title
#'
#' @param proj .
#' @param neib .
#' @param tau .
#'
#' @export
car_expl = function(proj, neib, tau) {
  list(cov = list(tau * car(neib(proj$space))))
}
