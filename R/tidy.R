#' Title
#'
#' @param proj .
#' @param info .
#'
#' @export
set_info = function(proj, info) {
  proj$info = info
  return(proj)
}


#' Title
#'
#' @param proj .
#' @param simdata .
#' @param ... .
#'
#' @export
set_data = function(proj, simdata = F, ...) {
  if (simdata) {
    proj$data = generate_data(...)
  } else {
    proj$data = process_data(...)
  }
  return(proj)
}


#' Title
#'
#' @param proj .
#' @param ... .
#'
#' @export
set_prior = function(proj, ...) {
  proj$prior = prior(proj$data, ...)
  return(proj)
}


#' Title
#'
#' @inheritParams set_prior
#'
#' @export
set_fit = function(proj, ...) {
  proj$fit = stan(proj, ...)
  return(proj)
}


#' Title
#'
#' @inheritParams set_prior
#'
#' @export
set_summary = function(proj, ...) {
  proj$summary = summary_matrix(proj$fit, proj$data)
  return(proj)
}
