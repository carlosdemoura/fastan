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


#' Export most common plots of a project
#'
#' @param proj fastan project
#' @param path_dump .
#' @param rds .
#' @param plot.extension .
#'
#' @export
#'
#' @import ggplot2
export = function(proj, path_dump = getwd(), rds = T, plot.extension = "png") {
  real = ifelse(!is.null(proj$data$real), T, F)
  path = paste0(path_dump, "/fastanExport_", format(Sys.time(), "%Y-%m-%d-%Hh%Mm%Ss"))

  if (dir.exists(path)) {
    stop("failed to create folder on specified directory")
  } else {
    dir.create(path, recursive = T)
  }

  img = function(file) {
    file.path(path, paste0(file, ".", plot.extension))
  }

  for (fac in 1:n.fac(proj)) {
    if (real) {
      plot_hpd(proj, "alpha", col = fac, stat = c("mean", "real"))
    } else {
      plot_hpd(proj, "alpha", col = fac, stat = c("mean"))
    }
    ggsave(img(paste0("alpha_hpd", "_", fac)), width = 15, height = 5, dpi = 300, bg = "white")
  }

  if (real) {
    plot_lambda(proj, stat = "real")
  } else {
    plot_lambda(proj)
  }
  ggsave(img("lambda_hpd"), width = 15, height = 5, dpi = 300, bg = "white")

  plot_contrast(proj, par = "alpha")
  ggsave(img("alpha_contrast_mean"), width = 8, height = 8, dpi = 300, bg = "white")
  plot_contrast(proj, par = "alpha", stat = "hpd_contains_0")
  ggsave(img("alpha_contrast_0"), width = 8, height = 8, dpi = 300, bg = "white")
  plot_contrast(proj, par = "lambda")
  ggsave(img("lambda_contrast_mean"), width = 15, height = 5, dpi = 300, bg = "white")
  if (real) {
    plot_contrast(proj, par = "alpha", stat = "real")
    ggsave(img("alpha_contrast_real"), width = 8, height = 8, dpi = 300, bg = "white")
    plot_contrast(proj, par = "lambda", stat = "real")
    ggsave(img("lambda_contrast_real"), width = 15, height = 5, dpi = 300, bg = "white")
  }

  plot_trace(proj, par = "lp__")
  ggsave(img("traceplot_lp"), width = 15, height = 5, dpi = 300, bg = "white")

  for (stat in c("Rhat", "n_eff", "geweke")) {
    p = plot_diagnostic(proj, stat)
    ggsave(plot = p, file = img(paste0("diganostic", "_", stat)), width = 7, height = 5, dpi = 300, bg = "white")
  }

  if (!is.null(proj$summary$pred)) {
    plot_missing(proj)
    ggsave(img("missing_pattern"), width = 8, height = 6, dpi = 300, bg = "white")
  }

  sink(file.path(path, "report.txt"))
  fastan_report(proj) |> cat()
  sink()

  if (real) {
    sink(file.path(path, "percentage_hits.txt"))
    percentage_hits(proj) |> print()
    sink()
  }

  if (rds) {
    saveRDS(proj, file.path(path, "proj.rds"))
  }

  return(path)
}


#' Title
#'
#' @param proj .
#'
#' @export
fastan_report = function(proj) {
  paste0(
    "Fastan project report",
    "\n\ninfo\t\t"    , proj$info,
    "\ndata dim.\t"   , proj$data$dim$row, " (row)   ", proj$data$dim$col, " (col)   ", n.fac(proj), " (fac)   ", proj$data$dim$group.n, " (grp)   ",
    "\n\nSTAN args"   ,
    "\nchains\t\t"    , length(proj$fit@stan_args),
    "\niter\t\t"      , proj$fit@stan_args[[1]]$iter,
    "\nwarmup\t\t"    , proj$fit@stan_args[[1]]$warmup,
    "\nthinning\t"    , proj$fit@stan_args[[1]]$thin,
    "\nseed\t\t"      , proj$fit@stan_args[[1]]$seed,
    "\n\nSTAN elapsed time (mins.)\n"
  ) |>
    paste0(paste0(capture.output(print(elapsed_time_table(proj$fit))), collapse = "\n"))
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


#' Title
#'
#' @param fit stan fit
#' @param round .
#'
#' @export
#'
#' @import rstan
elapsed_time_table = function(fit, round = 4) {
  (rstan::get_elapsed_time(fit) / 60) |>
    round(round) |>
    as.data.frame() |>
    {\(.) rbind(., colSums(.))}() |>
    {\(.) cbind(., rowSums(.))}() |>
    {\(.) `rownames<-`(., c(rownames(.)[-nrow(.)], "total"))}() |>
    {\(.) `colnames<-`(., c(colnames(.)[-3], "total"))}()
}


#' Title
#'
#' @param data .
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import stats
loglik = function(data) {
  data = validate_proj_arg(data, "data")

  stopifnot(
    "for now, can only calculate loglik with true value of parameters" = (!is.null(data$real)),
    "for now, can only calculate loglik without missings"              = is.null(data$pred)
    )

  loglik = 0
  alpha_lambda = data$real$alpha %*% data$real$lambda

  for (row_ in unique(data$x$row)) {
    x =
      data$x |>
      dplyr::filter(data$x$row == row_) |>
      dplyr::select(dplyr::all_of("value")) |>
      purrr::pluck(1)

    loglik_ =
      stats::dnorm(x, alpha_lambda[row_,], sqrt(data$real$sigma2[row_,]) * diag(length(x))) |>
      diag() |>
      prod() |>
      log()

    loglik = loglik + loglik_
  }

  loglik
}


#' Title
#'
#' @param smry .
#' @param stat .
#'
#' @export
mse = function(smry, stat = "mean") {
  mse = list()
  for (par in names(smry)) {
    mse[[par]] = sum((smry[[par]][,,"real"] - smry[[par]][,,stat])^2) / length(smry[[par]][,,stat])
  }
  mse
}
