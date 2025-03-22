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
#' @return
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
#' @param str .
#' @param width .
#' @param side .
#' @param ... .
#'
#' @return
#'
#' @examples
#'
#' @import stringr
adj_str = function(str, width = 20, side = "right", ...) {
  stringr::str_pad(str, width, side, ...)
}


#' Title
#'
#' @param file .
#'
#' @return
#' @export
#'
#' @examples
get_rds = function(file) {
  env = new.env()
  load(file, envir = env)
  x = ls(env)
  get(x, envir = env)
}


#' Title
#'
#' @param data .
#' @param info .
#'
#' @return
#'
#' @examples
#'
#' @export
fa_project = function(data, info = "") {
  list(
    info  = info,
    data  = data,
    model = process_conf(data)
    ) |>
    {\(.) c(., list(fit = run_stan(.$model))) }() |>
    {\(.) c(., list(summary = .$fit |> rstan::extract() |> summary_matrix() ))}()
}
# fa_project = function(data, info = "", ...) {
#   list(
#     info  = info,
#     data  = data
#     ) |>
#     {\(.) c(., list(model = process_conf(.$data, ...) )) }() |>
#     {\(.) c(., list(fit = run_stan(.$model, ...)      )) }() |>
#     {\(.) c(., list(summary = .$fit |> rstan::extract() |> summary_matrix() ))}()
# }


#' Title
#'
#' @param m matrix 3d, the 3rd dim will be alongated
#'
#' @return
#'
#' @examples
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
matrix_to_df = function(m) {
  m |>
    {\(x)
      do.call(rbind, lapply(dimnames(x)[[3]], function(slice) {
        x[, , slice, drop = FALSE] |>
          as.data.frame() |>
          {\(.) `colnames<-`(., 1:ncol(.)) }() |>
          {\(.) dplyr::mutate(., row = row.names(.)) }() |>
          {\(.) tidyr::pivot_longer(., names_to = "col", values_to = "v", cols = 1:ncol(x)) }() |>
          {\(.) dplyr::mutate(., stat = slice) }()
      }))
    }() |>
    tidyr::pivot_wider(values_from = "v", names_from = "stat")
}
