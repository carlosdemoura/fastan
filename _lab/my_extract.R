#' My extract
#'
#' @param object stanfit object
#'
#' @export
#'
#' @import rstan
#' @import utils
my_extract = function(object) {
  pars <- object@sim$pars_oi #|> head(-1)
  tidx <- rstan:::pars_total_indexes(object@sim$pars_oi,
                             object@sim$dims_oi,
                             object@sim$fnames_oi,
                             pars)

  n_kept <- object@sim$n_save
  chains = object@sim$chains
  iter = object@sim$iter

  fun1 <- function(par_i) {
    if (par_i == "lp__") {
      dim_par = c(1,1)
    } else {
      dim_par = object@sim$dims_oi[[par_i]]
    }

    sss <- do.call(cbind, lapply(tidx[[par_i]], rstan:::get_kept_samples2, object@sim))
    dim(sss) <- c(sum(n_kept), dim_par)  # max para o caso lp__
    dimnames(sss) <- list(iterations = NULL)
    dim(sss) = c(dim(sss)[1]/chains, chains, dim(sss) |> utils::tail(2))
    sss
  }

  slist <- lapply(pars, fun1)
  names(slist) <- pars
  slist
}


# pars_total_indexes <- function(names, dims, fnames, pars) {
#   starts <- calc_starts(dims)
#   par_total_indexes <- function(par) {
#
#     p <- match(par, fnames)
#     if (!is.na(p)) {
#       names(p) <- par
#       attr(p, "row_major_idx") <- p
#       return(p)
#     }
#     p <- match(par, names)
#     np <- num_pars(dims[[p]])
#     if (np == 0) return(NULL)
#     idx <- starts[p] + seq(0, by = 1, length.out = np)
#     names(idx) <- fnames[idx]
#     attr(idx, "row_major_idx") <- starts[p] + idx_col2rowm(dims[[p]]) - 1
#     idx
#   }
#   idx <- lapply(pars, FUN = par_total_indexes)
#   nulls <- sapply(idx, is.null)
#   idx <- idx[!nulls]
#   names(idx) <- pars[!nulls]
#   idx
# }
#
#
# calc_starts <- function(dims) {
#   len <- length(dims)
#   s <- sapply(unname(dims), function(d)  num_pars(d), USE.NAMES = FALSE)
#   cumsum(c(1, s))[1:len]
# }
#
# num_pars <- function(d) prod(d)
#
# idx_col2rowm <- function(d) {
#   len <- length(d)
#   if (0 == len) return(1)
#   if (1 == len) return(1:d)
#   idx <- aperm(array(1:prod(d), dim = d))
#   return(as.vector(idx))
# }
#
# get_kept_samples2 <- function(n, sim) {
#   lst <- vector("list", sim$chains)
#   for (ic in 1:sim$chains) {
#     if (sim$warmup2[ic] > 0)
#       lst[[ic]] <- sim$samples[[ic]][[n]]
#     else
#       lst[[ic]] <- sim$samples[[ic]][[n]]
#   }
#   do.call(c, lst)
# }



#' Generate initial values for STAN MCMC from another fit
#'
#' @param fit stanfit object.
#'
#' @return list; for each
#'
#' @export
init_from_fit = function(fit) {
  draws = my_extract(fit)
  correct_dimensions = function(x, par) {
    if(is.null(dim(x))) {
      if (par == "lambda"){
        answer = matrix(x, nrow = 1)
      } else {
        answer = matrix(x, ncol = 1)
      }
    } else {
      answer = x
    }
    return(answer)
  }

  init = list()
  for (chain in 1:dim(draws$alpha)[2]) {
    init[[chain]] = list()
    for (par in c("alpha", "lambda", "sigma2")) {
      init[[chain]][[par]] =
        draws[[par]] |>
        {\(.) .[dim(.)[1],chain,,] }() |>
        correct_dimensions(par)
    }
  }
  init
}
