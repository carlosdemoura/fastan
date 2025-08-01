#' Generate new `fastan::project`
#'
#' @param proj list to be turned into project, if NULL (default) a blank new project is returned.
#'
#' @export
project = function(proj = NULL) {
  if (is.null(proj)) {
    proj = list()
  }
  class(proj) = "project"
  return(proj)
}


#' Convert a object to fastan data format
#'
#' @param obj object to be turned into class in question.
#'
#' @export
to_data = function(obj) {
  class(obj) = "data"
  return(obj)
}


#' Convert a object to fastan prior format
#'
#' @inheritParams to_data
#'
#' @export
to_prior = function(obj) {
  class(obj) = "prior"
  return(obj)
}


#' Convert a object to fastan summary format
#'
#' @inheritParams to_data
#'
#' @export
to_summary = function(obj) {
  class(obj) = "summary"
  return(obj)
}
