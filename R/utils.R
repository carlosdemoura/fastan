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
#' @param export_proj .
#' @param plot.extension .
#'
#' @export
#'
#' @import ggplot2
#' @import cli
export = function(proj, export_proj = T, plot.extension = "png") {
  real = ifelse(!is.null(proj$data$real), T, F)
  md5 = cli::hash_obj_md5(proj)

  for (i in 1:100) {
    path = md5 |> substr(1,10) |> {\(.) paste0("proj_export_plots_", ., "_", i)}()
    if (!dir.exists(path)) {
      dir.create(path, recursive = T)
      break
    }
  }

  if (!dir.exists(path)) {
    stop("failed to create folder on current directory")
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

  if (export_proj) {
    saveRDS(proj, file.path(path, "proj.rds"))
  }
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
