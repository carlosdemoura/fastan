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
fiat_groups_limits = function(x) {
  list(
    c(1, (cumsum(x) + 1)[1:(length(x)-1)]),
    cumsum(x)
  )
}


#' Title
#'
#' @param data .
#'
#' @export
prop.missing = function(data) {
  data = validate_proj_arg(data, "data")
  mis = ifelse(!is.null(data$pred), nrow(data$pred), 0)
  mis / (nrow(data$x) + mis)
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
#' @importFrom gridExtra grid.arrange
export = function(proj, path_dump = getwd(), rds = T, plot.extension = "png") {
  real = !is.null(proj$data$real)
  path = paste0(path_dump, "/fastanExport-", format(Sys.time(), "%Y_%m_%d-%Hh%Mm%Ss"))

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
    lambda = plot_lambda(proj, stat = "real")
    sigma2 = plot_hpd(proj, "sigma2", col = 1, stat = c("mean", "real"))
  } else {
    lambda = plot_lambda(proj)
    sigma2 = plot_hpd(proj, "sigma2", col = 1, stat = "mean")
  }
  ggsave(plot = lambda, filename = img("lambda_hpd"), width = 15, height = 5, dpi = 300, bg = "white")
  ggsave(plot = sigma2, filename = img("sigma2_hpd"), width = 15, height = 5, dpi = 300, bg = "white")

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
    p_diag = plot_diagnostic(proj, stat)
    ggsave(plot = p_diag, filename = img(paste0("diganostic", "_", stat)), width = 7, height = 5, dpi = 300, bg = "white")
  }

  if (!is.null(proj$summary$pred)) {
    plot_missing(proj)
    ggsave(img("missing_pattern"), width = 8, height = 6, dpi = 300, bg = "white")
  }

  sink(file.path(path, "report.txt"))
  fastan_report(proj) |> cat()
  sink()

  if (real) {
    sink(file.path(path, "accuracy.txt"))
    accuracy(proj) |> print()
    sink()

    bias = list()
    for (param in c("all", names(proj$summary))) {
      bias[[param]] = plot_bias(proj, param)
    }
    p_bias = gridExtra::grid.arrange(grobs = bias, ncol=2)
    ggsave(plot = p_bias, filename = img("bias"), width = 7, height = 5, dpi = 300, bg = "white")
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


#' In hours
#'
#' @param fit stan fit
#'
#' @export
#'
#' @importFrom rstan get_elapsed_time
elapsed_time_table = function(fit) {
  fit = validate_proj_arg(fit, "fit")
  (rstan::get_elapsed_time(fit) / 3600) |>
    as.data.frame() |>
    {\(.) rbind(., colSums(.))}() |>
    {\(.) cbind(., rowSums(.))}() |>
    {\(.) `rownames<-`(., c(rownames(.)[-nrow(.)], "total"))}() |>
    {\(.) `colnames<-`(., c(colnames(.)[-3], "total"))}()
}


#' Title
#'
#' @param proj .
#' @param param .
#' @param stat .
#'
#' @export
#'
#' @import dplyr
#' @importFrom stats dnorm
loglik = function(proj, param = NULL, stat = "mean") {
  stopifnot(
    "only one of param/stat can be not null" = (is.null(param) | is.null(stat)) & !(is.null(param) & is.null(stat))
    )

  if (!is.null(param)) {
    alpha_lambda = param$alpha %*% param$lambda
    sigma2 = param$sigma2
  } else {  # if (!is.null(stat))
    if (stat == "real") {
      alpha_lambda = proj$data$real$alpha %*% proj$data$real$lambda
      sigma2 = proj$data$real$sigma2
    } else {
      alpha_lambda = proj$summary$alpha[,,stat] %*% proj$summary$lambda[,,stat]
      sigma2 = proj$summary$sigma2[,,stat] |> as.matrix()
    }
  }

  loglik = 0

  for (row_ in unique(proj$data$x$row)) {
    df =
      proj$data$x |>
      dplyr::filter(proj$data$x$row == row_)

    x = df$value
    cols = df$col

    loglik_ =
      stats::dnorm(x, alpha_lambda[row_,cols], sqrt(sigma2[row_,1]) * diag(length(x))) |>
      diag() |>
      prod() |>
      log()

    loglik = loglik + loglik_
  }

  loglik
}


#' Title
#'
#' @param proj fastan project
#'
#' @export
param.dim = function(proj) {
  stopifnot("project must have prior or summary" = ("prior" %in% names(proj)) | ("summary" %in% names(proj)))

  df =
    matrix(0, nrow = 4, ncol = 3) |>
    as.data.frame() |>
    `colnames<-`(c("row", "col", "total")) |>
    `rownames<-`(c("alpha", "lambda", "sigma2", "pred"))

  if ("summary" %in% names(proj)) {
    smry = proj$summary
    for (par in c("alpha", "lambda", "sigma2", "pred")) {
      df[par, ] =
        smry[[par]] |>
        {\(.) if (!is.null(.)) .[,,"mean"] |> as.matrix()
          else .}() |>
        {\(.) c(nrow(.), ncol(.), nrow(.) * ncol(.))}() |>
        {\(.) if (length(.)) .
          else rep(0, 3)}()
    }
  } else {
    fac = length(proj$prior$alpha$mean)
    row = length(proj$prior$alpha$mean[[1]])
    col = length(proj$prior$lambda$mean[[1]])
    df["alpha",]  = c(row, fac, row*fac)
    df["lambda",] = c(fac, col, fac*col)
    df["sigma2",] = c(row, 1, row)
    df["pred",]   = c(nrow(proj$data$pred), 1, nrow(proj$data$pred))
  }

  if ( ! ("pred" %in% names(proj$data) | "pred" %in% names(proj$summary)) ) {
    df = df[-4,]
  }
  df
}


#' Get percentage of parameters that are in HPD & mean relative bias from simdata
#'
#' @param smry fastan summary
#' @param correct .
#'
#' @export
#'
#' @importFrom stats weighted.mean
accuracy = function(smry, correct = F) {
  smry = validate_proj_arg(smry, "summary")
  stopifnot("data must be simdata" = "real" %in% dimnames(smry$alpha)[[3]])
  table =
    matrix(0, nrow = 4, ncol = 3) |>
    as.data.frame() |>
    `colnames<-`(c("p", "bias", "total")) |>
    `rownames<-`(c("alpha", "lambda", "sigma2", "pred"))
  for (par in names(smry)) {
    p =
      smry[[par]][,,"real"] |>
      {\(.) (. >= smry[[par]][,,"hpd_min"]) & (. <= smry[[par]][,,"hpd_max"])}() |>
      as.numeric() |>
      c()
    b = smry[[par]][,,"bias"] |> c()

    if ((par == "alpha") & correct) {
      p = p[as.logical(smry$alpha[,,"in_group"])]
      b = b[as.logical(smry$alpha[,,"in_group"])]
    }

    table[par, "p"] = mean(p)
    table[par, "bias"] = mean(b)
  }
  if (!("pred" %in% names(smry))) {
    table = table[-4,]
  }
  table$total = param.dim(list(summary = smry))$total
  table["all",] =
    stats::weighted.mean(table$p, table$total) |>
    c(stats::weighted.mean(table$bias, table$total)) |>
    c(sum(table$total))
  table
}
