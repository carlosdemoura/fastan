#' Generate sentinel values for your data
#'
#' Generate distinct sentinel values based on your data (generally the first multiple of 1000 one order above the entries of your argument).
#'
#' @param m A scalar/vector/matrix of numeric values.
#'
#' @return A integer.
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


#' Title
#'
#' @param proj .

#' @export
fastan_report = function(proj) {
  paste0(
    "Fastan project report",
    "\n\ninfo\t\t"    , proj$info,
    "\n\nFastan model",
    "\ntype\t\t"      ,
    "\ndata dim.\t"   ,
    "\n\nSTAN args"   ,
    "\nchains\t\t"    , length(proj$fit@stan_args),
    "\niter\t\t"      , proj$fit@stan_args[[1]]$iter,
    "\nwarmup\t\t"    , proj$fit@stan_args[[1]]$warmup,
    "\nthinning\t"    , proj$fit@stan_args[[1]]$thin,
    "\nseed\t\t"      , proj$fit@stan_args[[1]]$seed
  )
}
