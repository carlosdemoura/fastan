#' Title
#'
#' @param data matrix
#' @param value string
#' @param group string
#' @param row string
#' @param col string
#'
#' @return
#' About sentinel
#' * sentinel + 0 missing data for mcmc
#' * sentinel + 1 missing data, but do nothing
#' * sentinel + 2 data for test of adjustment
#'
#' @examples
#'
#' @export
#'
#' @import dplyr
process_conf = function(data, value, group, row, col) {
  levels = list(
    factor  = levels(data[[col]])  ,
    group   = levels(data[[group]]),
    loading = levels(data[[row]])
  )

  data_fa = data |>
    {\(.)
    dplyr::mutate(.,
      row    = match(.$row,   levels$loading),
      col    = match(.$col,   levels$factor ),
      group  = match(.$group, levels$group  )
    )}() |>
    dplyr::rename("value" = dplyr::all_of(value))

  coor = data_fa |>
    dplyr::select(all_of(c("row", "group"))) |>
    unique()

  var_alpha_prior =
    matrix(1e-2, nrow = max(data_fa$row), ncol = max(data_fa$group))
  var_alpha_prior[cbind(coor$row, coor$group)] = 10

  list(
    data = data,
    dim = list(al_row  = max(data_fa$row),
               al_col  = max(data_fa$col),
               al_fac  = max(data_fa$group),
               obs_row = nrow(data_fa),
               obs_col = ncol(data_fa)
               ),
    var_alpha_prior = var_alpha_prior,
    sentinel = fiat_sentinel(data_fa$value),
    levels = levels
  )
}


#' Title
#'
#' @param mod
#' @param p
#'
#' @return
#'
#' @examples
#'
#' @export
hide_data = function(mod, p) {
  n = floor(p * nrow(mod$data))
  s = sample(nrow(mod$data), size = n, replace = F) |>
    sort()

  mod$real$pred = mod$data[s,]

  mod$data[s,]$value = mod$sentinel + 3

  return(mod)
}
