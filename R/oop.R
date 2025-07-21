#' Generate new `fastan::project`
#'
#' @export
new_project = function() {
  proj = list()
  class(proj) = "project"
  return(proj)
}
