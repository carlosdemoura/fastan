#' tidy: set information in project
#'
#' @inheritParams set_summary
#' @param info string with info
#'
#' @export
set_info = function(proj, info) {
  proj$info = info
  return(proj)
}


#' tidy: set data in project
#'
#' @inheritParams set_summary
#' @param simdata logical/string, simdata type.
#' @param pred numeric/NULL, if data is simdata.
#' @param cicles numeric.
#' @param seed numeric.
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
      real = real_from_posterior(proj)
    } else if (simdata == "real") {
      real = dots$real
    } else {
      stop("simdata type not accepted")
    }
    data = generate_data(real = real, cicles = cicles)

    if (!is.null(proj$data$pred)) {
      data$pred = dplyr::left_join(proj$data$pred[c("row", "col")], data$obs, by = c("row", "col")) |> dplyr::relocate(dplyr::all_of("value"))
      data$obs = dplyr::right_join(data$obs, proj$data$obs[c("row", "col")], by = c("row", "col"))
    } else if (is.null(proj$data$pred) & !is.null(pred)) {
      stopifnot("it's necessary 0 <= pred < 1 " = (0 <= pred) & (pred < 1))
      if (pred > 0) {
        index = 1:nrow(data$obs) |> sample(size = floor(pred * nrow(data$obs)), replace = F) |> sort()
        data$pred = data$obs[index, ]
        data$obs = data$obs[-index, ]
      }
    }

  } else {
    data = process_data(...)
  }

  proj$data = data
  return(proj)
}


#' tidy: set space in project
#'
#' @inheritParams set_summary
#' @param type string, default = "real", if "random" a random set of coordinates is generated, if "real" a data.frame with coordinates is expected.
#' @param seed numeric, seed in case of simdata (only makes sense if type = "random"), if NULL (default) no seed is set.
#'
#' @export
set_space = function(proj, type = "real", seed = NULL, ...) {
  stopifnot("type must bet real or random" = type %in% c("real", "random"))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (type == "real") {
    proj$space = space_process(proj$data, ...)
  } else if (type == "random") {
    df =
      proj$data$label$loading |>
      length() |>
      generate_space(...) |>
      cbind(data.frame(id = proj$data$label$loading))
    proj$space = space_process(proj$data, df = df, label = "id", lat = "lat", lon = "lon", alt = "alt", position = "row")
  }

  return(proj)
}


#' tidy: set prior in project
#'
#' @inheritParams set_summary
#' @param type string, prior type.
#' @param semi.conf logical, `TRUE` if model is semi-confirmatory, `FALSE` otherwise.
#' @param engine list of engines to generate prior hyperparameters, if NULL (default) then prior is independent.
#'
#' @export
set_prior = function(proj, type = "normal", semi.conf, engine = NULL, ...) {
  if (type == "normal") {
    prior_ = prior_normal(proj$data, semi.conf = semi.conf, ...)
    proj_ = proj; proj_$prior = prior_
    prior = list(alpha = list(), lambda = list())
    for (par in c("alpha", "lambda", "sigma2")) {
      if (!is.null(engine[[par]])) {
        prior[[par]] = engine[[par]](proj_)
      }
      for (name in names(prior_[[par]])) {
        if (is.null(prior[[par]][[name]])) { prior[[par]][[name]] = prior_[[par]][[name]]}
      }
    }
  } else {
    stop("prior type not accepted")
  }

  prior[["semi.conf"]] = semi.conf
  prior[["type"]] = type

  proj$prior = to_prior(prior)
  return(proj)
}


#' tidy: set fit in project
#'
#' @inheritParams set_summary
#' @param set_summary logical, `TRUE` (default) for try to set summary, `FALSE` otherwise.
#' @param set_diagnostic logical, `TRUE` (default) for try to set diagnostic, `FALSE` otherwise.
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


#' tidy: set diagnostic in project
#'
#' @inheritParams set_summary
#'
#' @export
set_diagnostic = function(proj, ...) {
  try_set({
    proj$diagnostic = diagnostic(proj$fit)
  })
  return(proj)
}


#' tidy: set summary in project
#'
#' @param proj `fastan::project` object.
#' @param ... dot args.
#'
#' @export
set_summary = function(proj, ...) {
  try_set({
    proj$summary = summary_matrix(proj, ...)
  })
  return(proj)
}


#' Try set attribute in project and catch errors
#'
#' @param expr expression.
try_set = function(expr) {
  tryCatch({
    expr
  }, error = function(e){
    warning(paste("ERROR:", e))
  })
}


#' Remove attributes from `fastan::project`
#'
#' @param proj fastan project object
#' @param ... strings, attributes to be removed
#'
#' @export
delete = function(proj, ...) {
  attr = unlist(list(...))

  if (length(attr)) {
    for (x in attr) {
      proj[[x]] = NULL
    }
  }
  return(proj)
}


#' Missing validation
#'
#' @inheritParams set_summary
#'
#' @export
#'
#' @import dplyr
missing_validation = function(proj, ...) {
  proj$data = missing_validation_selection(proj)
  proj$space = dplyr::left_join(data.frame(id = proj$data$label$loading), proj$space)
  # space_rows =
  #   proj$data$label$loading |>
  #   lapply(function(x) which(x == proj$space$id, proj$space$id)) |>
  #   unlist()
  # proj$space = proj$space[space_rows,]
  proj = proj |> remove( setdiff(names(proj), c("info", "data", "space")) )
  return(proj)
}


#' Inver signal in factor
#'
#' @param proj `fastan::project` object.
#' @param fac integer vector with number the of factors to invert.
#' @param bias.stat sting, default = "mean", Bayes estimator to use to calculate bias, if data is simdata.
#'
#' @export
invert_signal = function(proj, fac, bias.stat = "mean") {
  smry = proj$summary

  stat = c("mean", "median")
  for (loc in fac) {
    smry$alpha[,loc,c(stat, "hpd_min", "hpd_max")]  = -smry$alpha[,loc,c(stat, "hpd_max", "hpd_min")]
    smry$lambda[loc,,c(stat, "hpd_min", "hpd_max")] = -smry$lambda[loc,,c(stat, "hpd_max", "hpd_min")]
  }

  for (parameter in c("alpha", "lambda")) {
    if ("real" %in% dimnames(smry[[parameter]])[[3]]) {
      denom = smry[[parameter]][,,"real"]
      denom[denom == 0] = 1
      smry[[parameter]][,,"bias"] = (smry[[parameter]][,,bias.stat] - smry[[parameter]][,,"real"]) / abs(denom)
    }
  }

  proj$summary = smry

  proj
}
