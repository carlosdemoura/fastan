#' Generate sentinel values for your data
#'
#' Generate distinct sentinel values based on your data (generally the first multiple of 1000 one order above the entries of your argument).
#'
#' @param m A scalar/vector/matrix of numeric values.
#'
#' @return A integer.
#'
#' @examples
#' fiat_sentinel(matrix(1:4, ncol = 2))
#'
#' @export
fiat_sentinel = function(m){
  stopifnot(is.numeric(m))
  return( (floor(abs(max(m)) / 100) + 1) * 1000 )
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


#' Generate data (confirmatory or semi-confirmatory models)
#'
#' @param rows.by.group integer vector; size (number of loadings - or rows in the wider format) of each group.
#' @param columns integer; number of columns in the wider format.
#' @param cicles integer; UNDER DEVELOPMENT.
#' @param semi.conf boolean:
#' * `TRUE` if it's semi-confirmatory (then the last group is considered to be the semi-confirmatory group);
#' * `FALSE` if it's confirmatory (DEFAULT).
#'
#' @return fastan model object
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
generate_data_sc = function(rows.by.group, columns, cicles = 1, semi.conf = F) {
  #rows.by.group = rep(10, 3); columns = 8; cicles = 1; semi.conf = T
  stopifnot(
    "if the model is semi.conf there
    must be at least three groups" = ifelse(semi.conf, length(rows.by.group) >= 3, T)
  )

  n.fac = length(rows.by.group) - as.integer(semi.conf)

  groups_limits = fiat_groups_limits(rows.by.group)

  alpha = matrix(0,
                 nrow = sum(rows.by.group),
                 ncol = n.fac)

  lambda = matrix(0,
                  ncol = columns,
                  nrow = n.fac)

  for (i in 1:n.fac) {
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = runif(rows.by.group[i], -5, 5)

    lambda[i, ] = sort(rnorm(columns, 0, 1), decreasing = as.logical(i %/% 2))
  }

  if (semi.conf) {
    i = i + 1
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], ] =
      matrix(
        runif(rows.by.group[i] * n.fac, -5, 5),
        nrow = rows.by.group[i],
        ncol = n.fac
      )
  }

  sigma2 = runif(sum(rows.by.group), .5, 5)

  epsilon = matrix(
    rnorm(sum(rows.by.group)*columns*cicles, 0, sqrt(sigma2)) ,
    ncol = columns*cicles,
    byrow = F
  )

  alpha_lambda = alpha %*% lambda |>
    {\(.)
      do.call(cbind, lapply(1:ncol(.), function(i) {
        matrix(rep(.[, i], cicles), nrow = nrow(.))
      }))
    }()

  x = (alpha_lambda + epsilon) |>
    as.data.frame() |>
    dplyr::mutate(
      row = paste0("row ", 1:sum(rows.by.group)),
      group = paste0("group ", 1:n.fac) |>
        {\(.) if(semi.conf)
          rep(., head(rows.by.group, -1)) |>
            c("group extra" |> rep(tail(rows.by.group, 1)))
          else
            rep(., rows.by.group)
        }()
    ) |>
    `colnames<-`(
      paste0("level ", 1:columns) |>
        rep(each = cicles) |>
        append(c("row", "group")) |>
        make.unique()
    ) |>
    tidyr::pivot_longer(cols = 1:{\()columns*cicles}() , names_to = "col", values_to = "value") |>
    {\(.)
      dplyr::mutate(.,
                    col = sapply(.$col, function(x) { strsplit(x, "[.]") |> purrr::pluck(1) |> purrr::pluck(1)}) |>
                      {\(.) factor(., levels = unique(.) )}(),
                    group = factor(.$group, levels = unique(.$group)),
                    row   = factor(.$row,   levels = unique(.$row))
      )}()

  mod = model_data_sc(x, "value", "group", "row", "col", semi.conf)
  mod$real = list(alpha = alpha,
                  lambda = lambda,
                  sigma2 = as.matrix(sigma2)
  )
  return(mod)
}


#' Process data in `fastan` model format (confirmatory or semi-confirmatory models)
#'
#' @param data data.frame in the longer format.
#' @param value string; name of the column containing values.
#' @param group string; name of the column containing groups.
#' @param row string; name of the column containing rows/loadings.
#' @param col string; name of the column containing columns/factor-levels.
#'
#' @return `fastan` model object
#' About sentinel
#' * sentinel + 0 missing data for mcmc
#' * sentinel + 1 missing data, but do nothing
#' * sentinel + 2 data for test of adjustment
#'
#' @export
#'
#' @import dplyr
model_data_sc = function(data, value, group, row, col, semi.conf, factor_name = NULL) {
  #data = x; row = "row"; group = "group"; col = "col"; value = "value"
  labels = list(
    factor_level = unique(data[[col]])  ,
    group        = unique(data[[group]]),
    loading      = unique(data[[row]])
  )

  if (!is.null(factor_name)) {
    labels$factor_name = factor_name
  } else {
    labels$factor_name = levels(data[[col]])
    if (semi.conf) {
      labels$factor_name = head(labels$factor_name, -1)
    }
  }

  data_fa = data |>
    dplyr::rename("value" = !!value,
                  "row"   = !!row,
                  "col"   = !!col,
                  "group" = !!group,
    ) |>
    {\(.) .[c("value", "row", "col", "group")]}() |>
    {\(.)
      dplyr::mutate(.,
                    row    = .$row   |> factor(labels$loading)      |> as.numeric(),
                    col    = .$col   |> factor(labels$factor_level) |> as.numeric(),
                    group  = .$group |> factor(labels$group)        |> as.numeric()
      )}()

  coor = data_fa |>
    dplyr::select(all_of(c("row", "group"))) |>
    unique()

  var_alpha_prior =
    matrix(1e-2, nrow = max(data_fa$row), ncol = max(data_fa$group))
  var_alpha_prior[cbind(coor$row, coor$group)] = 10

  if (semi.conf) {
    sc_coor =
      data_fa |>
      dplyr::filter( group == length(labels$group)  ) |>
      dplyr::select(all_of(c("row"))) |>
      unique() |>
      c() |>
      unlist() |>
      unname()

    var_alpha_prior = var_alpha_prior[,1:(max(data_fa$group) - as.numeric(semi.conf))]
    var_alpha_prior[sc_coor, ] = 10
  }

  list(
    data = data_fa,
    dim = list(al_row  = max(data_fa$row),
               al_col  = max(data_fa$col),
               al_fac  = max(data_fa$group) - as.numeric(semi.conf),
               obs_row = nrow(data_fa),
               obs_col = ncol(data_fa)
    ),
    var_alpha_prior = var_alpha_prior,
    sentinel = fiat_sentinel(data_fa$value),
    labels = labels
  )
}


#' Process data in `fastan` model format (cluster model)
model_data_cluster = function() {

}


#' My extract
#'
#' @export
my_extract = function(object) {
  pars <- object@sim$pars_oi #|> head(-1)
  tidx <- pars_total_indexes(object@sim$pars_oi,
                             object@sim$dims_oi,
                             object@sim$fnames_oi,
                             pars)

  n_kept <- object@sim$n_save
  chains = object@sim$chains
  iter = object@sim$iter

  fun1 <- function(par_i) {
    if (par_i == "lp__") {
      dim_par = c(1,1)
    } else {
      dim_par = object@sim$dims_oi[[par_i]]
    }

    sss <- do.call(cbind, lapply(tidx[[par_i]], get_kept_samples2, object@sim))
    dim(sss) <- c(sum(n_kept), dim_par)  # max para o caso lp__
    dimnames(sss) <- list(iterations = NULL)
    dim(sss) = c(dim(sss)[1]/chains, chains, dim(sss) |> utils::tail(2))
    sss
  }

  slist <- lapply(pars, fun1)
  names(slist) <- pars
  slist
}


pars_total_indexes <- function(names, dims, fnames, pars) {
  starts <- calc_starts(dims)
  par_total_indexes <- function(par) {

    p <- match(par, fnames)
    if (!is.na(p)) {
      names(p) <- par
      attr(p, "row_major_idx") <- p
      return(p)
    }
    p <- match(par, names)
    np <- num_pars(dims[[p]])
    if (np == 0) return(NULL)
    idx <- starts[p] + seq(0, by = 1, length.out = np)
    names(idx) <- fnames[idx]
    attr(idx, "row_major_idx") <- starts[p] + idx_col2rowm(dims[[p]]) - 1
    idx
  }
  idx <- lapply(pars, FUN = par_total_indexes)
  nulls <- sapply(idx, is.null)
  idx <- idx[!nulls]
  names(idx) <- pars[!nulls]
  idx
}


calc_starts <- function(dims) {
  len <- length(dims)
  s <- sapply(unname(dims), function(d)  num_pars(d), USE.NAMES = FALSE)
  cumsum(c(1, s))[1:len]
}

num_pars <- function(d) prod(d)

idx_col2rowm <- function(d) {
  len <- length(d)
  if (0 == len) return(1)
  if (1 == len) return(1:d)
  idx <- aperm(array(1:prod(d), dim = d))
  return(as.vector(idx))
}

get_kept_samples2 <- function(n, sim) {
  lst <- vector("list", sim$chains)
  for (ic in 1:sim$chains) {
    if (sim$warmup2[ic] > 0)
      lst[[ic]] <- sim$samples[[ic]][[n]]
    else
      lst[[ic]] <- sim$samples[[ic]][[n]]
  }
  do.call(c, lst)
}

#library(ggplot2)
#ibrary(ggridges)

#' Plot contrast matrix
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_contrast = function(smry, par = "alpha", stat = "mean") {
  df = smry[[par]][,,stat, drop=F] |>
    as.data.frame() |>
    {\(.) `colnames<-`(., 1:ncol(.))}() |>
    {\(.) dplyr::mutate(., row = as.numeric(rownames(.))) }() |>
    tidyr::pivot_longer(cols = -row, names_to = "factor", values_to = "value")

  if (stat == "hpd_contains_0") {
    df$value = ifelse(df$value, "No", "Yes")
  }

  ggplot(df, aes(factor, row, fill = value)) +
    geom_tile() +
    {if (stat == "hpd_contains_0") {
      scale_fill_manual(values = c("Yes" = "black", "No" = "white"),
                        guide = guide_legend(override.aes = list(color = "black", size = 5, stroke = 1)) )
    } else {
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
    }
    } +
    scale_y_reverse() +
    labs(fill = ifelse(stat == "hpd_contains_0", "Factor is significative", stat),
         x = "Factor",
         y = "Row",
         title = "Alpha posterior contrast")
}


#' Plot HPD interval & other stats
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_hpd = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = matrix_to_df(smry[[par]])
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()
  df = df[df[[loc.name]] == get(loc.name), ]
  real = "real" %in% colnames(df)

  y_min = min(smry[[par]][,,"hpd_min"])
  y_max = max(smry[[par]][,,"hpd_max"])

  ggplot(df) +
    {if (loc.name == "row") { aes(x = col) }
      else { aes(x = row) } } +
    geom_errorbar(aes(ymin = hpd_min, ymax = hpd_max, color = "HPD"), width = .6, linewidth = .6) +
    {if ("median" %in% stat) geom_point(aes(y = median, color = "Median"), size = 2, shape = 17)} +
    {if ("mean"   %in% stat) geom_point(aes(y = mean  , color = "Mean")  , size = 2, shape = 16)} +
    {if ("real"   %in% stat) geom_point(aes(y = real  , color = "Real")  , size = 2, shape = 15)} +
    labs(
      x = ifelse(loc.name == "row", "col", "row"),
      y = "Values",
      title = paste(par, "posterior", loc.name, c(row, col)),
      color = "Legend"
    ) +
    ylim(y_min, y_max) +
    {if (real) { scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue", "Real" = "green")) }
      else { scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue")) } } +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}


#' Plot lambdas
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_lambda = function(smry, stat = "mean") {
  df = matrix_to_df(smry$lambda)

  ggplot(df, aes(x = col, group = factor(row), color = factor(row), fill = factor(row))) +
    geom_ribbon(aes(ymin = hpd_min, ymax = hpd_max), alpha = 0.2, color = NA) +
    {if ("mean" %in% stat)   geom_line(aes(y = mean)  , size = 1)} +
    {if ("median" %in% stat) geom_line(aes(y = median), size = 1)} +
    {if ("real" %in% stat)   geom_line(aes(y = real)  , size = 1, color = "green")} +
    scale_color_brewer(name = "Factor", palette = "Set1") +
    scale_fill_brewer(name = "Factor", palette = "Set1") +
    labs(
      title = "lambda posterior (HPD & mean)",
      x = "Column",
      y = "Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))
}


#' Plot posterior sample histogram and/or density
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_posterior = function(fit, par, row = 1, col = 1, type = c("hist", "dens")) {
  if (class(fit) == "stanfit") {
    draws = rstan::extract(fit)[[par]]
  } else {
    draws = fit[[par]]
    dim(draws) =
      dim(draws) |>
      {\(.) c(prod(utils::head(., -2)), utils::tail(., 2))}()
  }

  df = draws |>
    {\(.) .[,row,col]}() |>
    as.data.frame() |>
    `colnames<-`("x")

  if (par != "lp__") {
    y_max = list(
      {ggplot(df, aes(x = x)) + geom_histogram(aes(y = ..density..), binwidth = 1)},
      {ggplot(df, aes(x = x)) + geom_density()}
    ) |>
      sapply( function(x){ x |> {\(.) ggplot_build(.)$data}() |> {\(.) max(.[[1]]$ymax)}() } ) |>
      max()
  }

  ggplot(df, aes(x = x)) +
    {if ("hist" %in% type) geom_histogram(aes(y = ..density..), binwidth = 1, fill = "grey")} +
    {if ("dens" %in% type) geom_density(lwd = 1.5, col = "red")} +
    #{if ("real" %in% type) geom_density(lwd = 1.5, col = "red")} +
    {if (par != "lp__") ylim(0, y_max)} +
    xlim(floor(min(df$x)), ceiling(max(df$x))) +
    theme_minimal()
}


#' Posterior sample trace plot
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
#' @import rstan
plot_trace = function(draws, par, row = 1, col = 1){
  df =
    draws |>
    {\(.) .[[par]][,,row,col]}() |>
    as.data.frame() |>
    {\(.) `colnames<-`(., paste("chain", 1:ncol(.))) }() |>
    {\(.) dplyr::mutate(., iteration = 1:nrow(.) )}() |>
    tidyr::pivot_longer(names_to = "chain", values_to = "value", cols = tidyr::starts_with("chain"))

  ggplot(df, aes(x = iteration, y = value, color = chain)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = paste0("MCMC for ", ifelse(par == "lp__", "Log-likelihood", paste0(par, "[", row, ",", col, "]"))),
      x = "Iteration",
      y = "Value"
    ) +
    theme_minimal()
}


#' FALTA MUITO
#'
#' Plot ridges
#'
#' @param model .
#'
#' @return ggridges plot
#'
#' @export
#'
#' @import ggplot2
#' @import ggridges
plot_ridges = function(model) {
  # FALTA AGRUPAR POR LINHA DATA FA
  ggplot(model$data, aes(x = value, y = factor(row))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    theme_minimal()
}


#' ESSA PORRA TÁ CERTA?
#'
#' Print LaTeX table
#'
#' @inheritParams plot_mock_doc
#'
#' @export
#'
#' @import ggplot2
print_table = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = matrix_to_df(smry[[par]])
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()
  df = df[df[[loc.name]] == get(loc.name), c("row", "col", stat)]
  df$loc = get(loc.name)
  real = "real" %in% colnames(df)

  cat(
    "\\begin{center}",
    paste("Summary of the MCMC for the posterior distribution of", par, ifelse(loc.name == "col", "column", "row"), c(row, col), "\\\\"),
    paste0(c("\\begin{tabular}{|", rep("m{1.5cm}|", length(stat)), "}"), collapse = ""),
    "\\hline",
    sep = "\n")

  cat("\\textbf{", ifelse(loc.name != "col", "Column", "Row") ,"}", sep = "")
  for (each in stat) {
    if (each == "real"   ){cat(" & \\textbf{Real value}"    , sep = "")
    } else if (each == "mean"   ){cat(" & \\textbf{Mean}"          , sep = "")
    } else if (each == "median" ){cat(" & \\textbf{Median}"        , sep = "")
    } else if (each == "sd"     ){cat(" & \\textbf{St dev}"        , sep = "")
    } else if (each == "hpd_min"){cat(" & \\textbf{HPD LB}"        , sep = "")
    } else if (each == "hpd_max"){cat(" & \\textbf{HPD UP}"        , sep = "")
    } else if (each == "hpd_amp"){cat(" & \\textbf{HPD amplitude}" , sep = "")}
  }
  cat(
    "\\\\",
    "\\hline",
    sep = "\n")

  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){

      cat(round(as.numeric(df[i,j]), 2))
      if (j < ncol(df)){
        cat(" & ")
      }

    }
    cat("\\\\\n\\hline\n")
  }

  cat(
    "\\end{tabular}",
    "\\end{center}",
    sep = "\n")
}


#' Transform 3D matrix into elongated data frame
#'
#' @param m matrix 3d, the 3rd dim will be elongated.
#'
#' @return `data.frame()`
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
matrix_to_df = function(m) {
  m |>
    {\(x)
      do.call(rbind, lapply(dimnames(x)[[3]], function(slice) {
        x[, , slice, drop = FALSE] |>
          as.data.frame() |>
          {\(.) `colnames<-`(., 1:ncol(.)) }() |>
          {\(.) dplyr::mutate(., row = row.names(.)) }() |>
          {\(.) tidyr::pivot_longer(., names_to = "col", values_to = "v", cols = 1:ncol(x)) }() |>
          {\(.) dplyr::mutate(., stat = slice) }()
      }))
    }() |>
    tidyr::pivot_wider(values_from = "v", names_from = "stat") |>
    {\(.) dplyr::mutate(
      .,
      row = as.numeric(.$row),
      col = as.numeric(.$col)
    )}()
}


#' Mock function for the sole purpose of documentation
#'
#' @param fit .
#' @param draws .
#' @param smry .
#' @param mod .
#' @param par .
#' @param row .
#' @param col .
#' @param type .
#' @param stat .
#' @param warmup .
plot_mock_doc = function() {}

#' FALTA PREDICTION
#'
#' Generate initial values for STAN MCMC
#'
#' @param model fastanModel object.
#' @param chains integer; number of chains.
#'
#' @return list; for each
#'
#' @export
fiat_init = function(model, chains) {
  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(1, nrow = model$dim$al_row,  ncol = model$dim$al_fac),
                     lambda = matrix(0, nrow = model$dim$al_fac,  ncol = model$dim$al_col),
                     sigma2 = matrix(1, nrow = model$dim$al_row,  ncol = 1)
                     #,
                     #pred   = matrix(param$pred[i]  , nrow = model$dim$obs_row, ncol = model$dim$obs_col)
    )

    if(is.null(T)) {
      init[[1]]$pred = matrix(as.numeric(pred), nrow = data$obs_row, ncol = data$obs_col)
    }
  }

  return(init)
}


#' Adjust the data argument on `rstan::stan()`
#'
#' @param model fastan model object.
#'
#' @return list; goes on the data argument in `rstan::stan()`.
#'
#' @export
#'
#' @import purrr
adjust_data_interface = function(model) {
  list(
    obs_arg         = model$data[,1] |> as.vector() |> unname() |> purrr::pluck(1),
    obs_coor        = model$data[,2:3] |> as.matrix() |> unname(),
    al_row          = model$dim$al_row,
    al_col          = model$dim$al_col,
    al_fac          = model$dim$al_fac,
    obs_row         = model$dim$obs_row,
    obs_col         = model$dim$obs_col,
    var_alpha_prior = model$var_alpha_prior,
    sentinel        = model$sentinel
  )
}


#' Run STAN MCMC
#'
#' @param model .
#' @param init .
#' @param chains .
#' @param pred .
#' @param ... arguments that will be passed to `rstan::stan()`
#'
#' @return rstan::stan fit object.
#'
#' @export
#'
#' @import rstan
run_stan = function(model, init = NULL, chains = 1, pred = F, ...) {
  if (is.null(init)) {
    init = fiat_init(model, chains)
  }
  rstan::stan(file   = file.path("inst/stan/interface_fa_sc.stan"),
              data   = adjust_data_interface(model),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
  )
}


#' Get summary from `rstan::stan()` fit object
#'
#' @param fit `rstan::stan()` fit object.
#' @param model fastan model object
#'
#' @return list of matrices.
#'
#' @export
#'
#' @import abind
#' @import coda
#' @import rstan
summary_matrix = function(fit, model = NULL) {
  if (class(fit) == "stanfit") {
    samp = rstan::extract(fit)
  } else {
    samp = fit |>
      lapply(function(x) {dim(x) = c(prod(utils::head(dim(x), -2)), utils::tail(dim(x), 2)); x})
  }

  matrices = list()

  for (parameter in setdiff(names(samp), "lp__") ) {

    hpd_temp = apply( samp[parameter][[1]], c(2,3),
                      function(x) {
                        coda::HPDinterval(coda::as.mcmc(x))[,c("lower", "upper")] |>
                          as.numeric() |>
                          list()
                      })

    matrices[[parameter]] = list(
      "mean"    = apply( samp[parameter][[1]], c(2,3), mean   )                          ,
      "median"  = apply( samp[parameter][[1]], c(2,3), median )                          ,
      "sd"      = apply( samp[parameter][[1]], c(2,3), sd     )                          ,
      "hpd_min" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(1) }) ,
      "hpd_max" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> purrr::pluck(2) }) ,
      "hpd_amp" = apply( hpd_temp, c(1,2), function(x) { unlist(x) |> diff() })
    )

    if (parameter == "alpha") {
      matrices[[parameter]][["hpd_contains_0"]] = ifelse((matrices[[parameter]][["hpd_max"]] >= 0) & (matrices[[parameter]][["hpd_min"]] <= 0), T, F)
    }

    if (!is.null(model) & !is.null(model$real)) {
      matrices[[parameter]][["real"]] = model[["real"]][[parameter]]
    }
  }

  sapply(matrices, function(x) { abind::abind(x, along = 3) })
}


#' Get diagnostic statistics from fit
#'
#' @import dplyr
#' @import tidyr
diagnostic_fit = function(fit) {
  df =
    summary(fit)$summary |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., par = row.names(.))}() |>
    dplyr::select(dplyr::all_of(c("n_eff", "Rhat", "par"))) |>
    dplyr::rename(neff = "n_eff", rhat = "Rhat") |>
    tidyr::extract(par, into = c("par", "row", "col"), regex = "([a-zA-Z0-9_]+)\\[(\\d+),(\\d+)\\]", convert = TRUE)
  df[nrow(df),] = c(df[nrow(df),1:2], "lp__", 1, 1)
  df
}


#' Get diagnostic statistics from draws
#'
#' @import dplyr
#' @import tidyr
diagnostic_draws = function(draws) {
  draws
}


#' Invert the signal of a column of lambda on the MCMC
invert_lambda_signal = function() {

}


#' Generate initial values for STAN MCMC from another fit
#'
#' @param draws fastanModel object.
#'
#' @return list; for each
#'
#' @export
fiat_init_from_draws = function(draws) {
  correct_dimensions = function(x, par) {
    if(is.null(dim(x))) {
      if (par == "lambda"){
        answer = matrix(x, nrow = 1)
      } else {
        answer = matrix(x, ncol = 1)
      }
    } else {
      answer = x
    }
    return(answer)
  }

  init = list()
  for (chain in 1:dim(draws$alpha)[2]) {
    init[[chain]] = list()
    for (par in c("alpha", "lambda", "sigma2")) {
      init[[chain]][[par]] =
        draws[[par]] |>
        {\(.) .[dim(.)[1],chain,,] }() |>
        correct_dimensions(par)
    }
  }
  init
}

#' Generate initial values for STAN MCMC from another fit
#'
#' @param draws1,draws2 draws
#'
#' @export
#'
#' @import abind
merge_draws = function(draws1, draws2) {
  x = sapply(
    list("alpha", "lambda", "sigma2"),
    function(x) {
      abind::abind(draws1[[x]], draws2[[x]], along = 1)
    })
  names(x) = c("alpha", "lambda", "sigma2")
  x
}

