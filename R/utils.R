#' Generate sentinel values for your data
#'
#' Generate distinct sentinel values based on your data (generally the first multiple of 1000 one order above the entries of your argument).
#'
#' @param m A scalar/vector/matrix of numeric values.
#'
#' @return A integer.
#'
#' @examples
#' fiat_sentinel(matrix(1:4, ncol = 2))
#'
#' @export
fiat_sentinel = function(m){
  stopifnot(is.numeric(m))
  return( (floor(abs(max(m)) / 100) + 1) * 1000 )
}


#' Title
#'
#' @param x .
#'
#' @export
fiat_groups_limits = function(x) {
  y = list(c(1, (cumsum(x) + 1)[1:(length(x)-1)]),
           cumsum(x)
  )
  return(y)
}


#' Get where data is stored
#'
#' Get where on your PC the package data is being stored.
#'
#' @return A string, folder path.
#'
#' @export
#'
#' @import tools
#' @import purrr
local_data = function(folder = NULL) {
  tools::R_user_dir("fastan", which = "data") |>
    {\(.) gsub("\\\\", "/", .)}() |>
    purrr::pluck(1) |>
    {\(.) if (is.null(folder)) file.path(.)
      else file.path(., folder)}()
}

if (!dir.exists(local_data("projects"))) dir.create(local_data("projects"), recursive = TRUE)


#' Title
#'
#' @param file .
#'
#' @export
get_rds = function(file) {
  env = new.env()
  load(file, envir = env)
  x = ls(env)
  get(x, envir = env)
}

