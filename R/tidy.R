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
#' @param pred .
#' @param cicles .
#' @param seed .
#' @param ... .
#'
#' @export
#'
#' @import dplyr
set_data = function(proj, simdata = "", pred = NULL, cicles = 1, seed = NULL, ...) {
  dots = match.call(expand.dots = FALSE)$...

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (nchar(simdata)) {

    if ((simdata == "dist") | identical(simdata, T)) {
      if (!is.null(proj$data)) {
        semi.conf = ifelse(is.null(dots$semi.conf), proj$prior$semi.conf, dots$semi.conf)
        real = real_from_dist(group.sizes = proj$data$dim$group.sizes, columns = proj$data$dim$col, semi.conf = semi.conf, ...)
      } else {
        real = real_from_dist(...)
      }
    } else if (simdata == "prior") {
      real = real_from_prior(proj)
    } else if (simdata == "posterior") {
      real = real_from_posterior(proj, ...)
    } else if (simdata == "real") {
      real = dots$real
    } else {
      stop("simdata type not accepted")
    }
    data = generate_data(real = real, cicles = cicles)

    if (!is.null(proj$data$pred)) {
      data$pred = dplyr::left_join(proj$data$pred[c("row", "col")], data$x, by = c("row", "col")) |> dplyr::relocate(dplyr::all_of("value"))
      data$x = dplyr::right_join(data$x, proj$data$x[c("row", "col")], by = c("row", "col"))
    } else if (is.null(proj$data$pred) & !is.null(pred)) {
      stopifnot("it's necessary 0 <= pred < 1 " = (0 <= pred) & (pred < 1))
      if (pred > 0) {
        index = 1:nrow(data$x) |> sample(size = floor(pred * nrow(data$x)), replace = F) |> sort()
        data$pred = data$x[index, ]
        data$x = data$x[-index, ]
      }
    }

  } else {
    data = process_data(...)
  }

  proj$data = data
  return(proj)
}


#' Title
#'
#' @param proj .
#' @param type .
#' @param ... .
#'
#' @export
set_prior = function(proj, type, ...) {
  if (type == "normal") {
    proj$prior = prior_normal(proj$data, ...)
  } else {
    stop("prior type not accepted")
  }
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
    proj$summary = summary_matrix(proj, ...)
  })
  return(proj)
}


#' Title
#'
#' @param proj .
#' @param attr .
#'
#' @export
remove = function(proj, attr) {
  for (x in attr) {
    proj[[x]] = NULL
  }
  return(proj)
}


#' Title
#'
#' @param expr .
try_set = function(expr) {
  tryCatch({
    expr
  }, error = function(e){
    cat(paste("ERROR:", e))
  })
}
