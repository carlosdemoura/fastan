#' Title
#'
#' @export
blank_project = function() {
  proj = list()
  class(proj) = "project"
  return(proj)
}
