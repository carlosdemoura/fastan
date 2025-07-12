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
#' @param ... .
#'
#' @export
set_space = function(proj, ...) {
  proj$space = space_process(proj$data, ...)

  return(proj)
}


#' Title
#'
#' @inheritParams set_space
#' @param type .
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
#' @inheritParams set_space
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
#' @inheritParams set_space
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
#' @inheritParams set_space
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
#' @inheritParams set_space
#'
#' @export
#'
#' @import dplyr
missing_validation = function(proj, ...) {
  proj$data = missing_validation_selection(proj)
  space_rows =
    proj$data$label$loading |>
    lapply(function(x) which(x == proj$space$id, proj$space$id)) |>
    unlist()
  proj$space = proj$space[space_rows,]
  proj = proj |> remove( setdiff(names(proj), c("info", "data", "space")) )
  return(proj)
}


#' Title
#'
#' @param proj .
#' @param ... .
#'
#' @export
remove = function(proj, ...) {
  if (lapply(list(...), is.character) |> unlist() |> all()) {
    attr = unlist(list(...))
  } else {
    attr = sapply(substitute(list(...))[-1], deparse)
  }

  if (length(attr)) {
    for (x in attr) {
      proj[[x]] = NULL
    }
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
