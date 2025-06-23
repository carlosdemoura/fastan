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
    if (is.null(proj$data)) {
      proj$data = generate_data(...)
    } else {
      #semi.conf = ifelse(!is.null(proj$prior), proj$prior$semi.conf, F)
      proj$data = generate_data_from_project(proj, ...)
    }
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
#' @param set_summary .
#' @param set_diagnostic .
#'
#' @export
set_fit = function(proj, set_summary = T, set_diagnostic = T, ...) {
  proj$fit = stan(proj, ...)

  if (set_diagnostic) {
    proj = set_diagnostic(proj)
  }
  if (set_summary) {
    proj = set_summary(proj)
  }
  return(proj)
}


#' Title
#'
#' @inheritParams set_prior
#'
#' @export
set_diagnostic = function(proj, ...) {
  try_set({
    proj$diagnostic = diagnostic(proj$fit)
  })
  return(proj)
}


#' Title
#'
#' @inheritParams set_prior
#'
#' @export
set_summary = function(proj, ...) {
  try_set({
    proj$summary = summary_matrix(proj$fit, proj$data)
  })
  return(proj)
}


#' Title
#'
#' @param expr
try_set = function(expr) {
  tryCatch({
    expr
  }, error = function(e){
    cat(paste("ERROR:", e))
  })
}
