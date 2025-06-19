#' Get number of factors of a model
#'
#' @param proj fastan project
#'
#' @return A integer.
#'
#' @export
n.fac = function(proj){
  proj$data$dim$group.n - as.integer(proj$prior$semi.conf)
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


#' Title
#'
#' @param obj .
#' @param class .
validate_proj_arg = function(obj, class) {
  if (inherits(obj, "project")) {
    return(obj[[class]])
  } else {
    return(obj)
  }
}
