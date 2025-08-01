#' Generate simulated data
#'
#' (exploratory, confirmatory or semi-confirmatory models)
#'
#' @param real list containing real values of the parameters and group sizes.
#' @param cicles integer, default 1, number of cicles.
#'
#' @return `fasta::data` object.
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @importFrom tidyr pivot_longer
#' @importFrom utils head tail
generate_data = function(real, cicles = 1) {
  group.sizes = real$group.sizes
  real$group.sizes = NULL
  n.fac = nrow(real$lambda)
  columns = ncol(real$lambda)
  semi.conf = n.fac != length(group.sizes)

  alpha_lambda =
    real$alpha %*% real$lambda |>
    {\(.)
      do.call(cbind, lapply(1:ncol(.), function(i) {
        matrix(rep(.[, i], cicles), nrow = nrow(.))
      }))
    }()

  epsilon =
    real$sigma2 |>
    {\(.) matrix(rep(., cicles*columns), nrow = nrow(.)) }() |>
    {\(.) lapply(seq_len(nrow(.)), function(i) (.[i,] |> diag()))}() |>
    {\(.) dist_multivariate_normal(lapply(., function(x) rep(0, nrow(x))) , .)}() |>
    generate(1) |>
    {\(.) do.call(rbind, .)}()

  x =
    (alpha_lambda + epsilon) |>
    as.data.frame() |>
    dplyr::mutate(
      row = paste0("row ", 1:sum(group.sizes)),
      group = paste0("group ", 1:n.fac) |>
        {\(.) if(semi.conf)
          rep(., utils::head(group.sizes, -1)) |>
            c("group extra" |> rep(utils::tail(group.sizes, 1)))
          else
            rep(., group.sizes)
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

  data = process_data(data = x, value = "value", row = "row", col = "col", group = "group")
  data$real = real

  data
}


#' Generate real value list from distributions
#'
#' Used on `fastan::generate_data`
#'
#' @param group.sizes integer vector with size of each group.
#' @param columns integer, number of columns.
#' @param semi.conf logical, `TRUE` if the model is semi-confirmatory, `FALSE` otherwise, NULL (default) means the code will infer the model is not semi.conf if the number of groups is <= 2
#' @param dist list of distributions from `distributional` from which the parameters will be sampled.
#'
#' @return real value list.
#'
#' @import distributional
#' @importFrom mvtnorm is.chol
#' @importFrom stats rbeta
real_from_dist = function(group.sizes, columns, semi.conf = NULL, dist = list()) {

  if (is.null(semi.conf)) {
    if (length(group.sizes) %in% 1:2) {
      semi.conf = F
    } else {
      stop("semi.conf must be specified if the number of groups is greater than 2")
    }
  } else {
    stopifnot(
      "if the model is semi.conf there
      must be at least three groups" = ifelse(semi.conf, length(group.sizes) >= 3, T)
    )
  }

  if (F) { mvtnorm::is.chol(1) }  # just for import mvtnorm, i did that bc distributional relies on mvtnorm for multivariate normal draws

  dist_ = list( alpha = dist_uniform(-6,6), lambda = dist_normal(0,1), sigma2 = dist_uniform(.1,3) )
  for (par in c("alpha", "lambda", "sigma2")) {
    if (is.null(dist[[par]])) {
      dist[[par]] = dist_[[par]]
    }
  }

  n.fac = length(group.sizes) - as.integer(semi.conf)
  group_limits = group_limits(group.sizes)

  alpha = matrix(0,
                 nrow = sum(group.sizes),
                 ncol = n.fac)

  lambda = matrix(0,
                  ncol = columns,
                  nrow = n.fac)

  for (i in 1:n.fac) {
    alpha[group_limits[[1]][i] : group_limits[[2]][i], i] = rep(1, group.sizes[i])
    lambda[i,] = generate(dist$lambda, columns)[[1]] |> matrix(ncol = columns) |> {\(.) .[1,]}()
  }

  if (semi.conf) {
    i = i + 1
    alpha[group_limits[[1]][i] : group_limits[[2]][i], ] =
      replicate(
        group.sizes[i],
        stats::rbeta(n.fac, 0.1, 0.2) |> {\(.) ./sum(.)}()
      ) |>
      t() |>
      {\(.) ifelse(. < 5e-2, 0, .)}()
  }

  alpha_ = do.call(rbind,
                   rep(generate(dist$alpha, 1), n.fac)) |> t()
  if (any(dim(alpha) != dim(alpha_))) {
    alpha_ = do.call(cbind,
                     rep(generate(dist$alpha, sum(group.sizes)), n.fac))
  }

  alpha = alpha * alpha_

  sigma2 =
    generate(dist$sigma2, sum(group.sizes))[[1]] |>
    matrix(ncol = sum(group.sizes)) |>
    {\(.) .[1,]}() |>
    as.matrix()

  list(alpha = alpha, lambda = lambda, sigma2 = sigma2, group.sizes = group.sizes, dist = dist)
}


#' Generate real value list from prior
#'
#' Used on `fastan::generate_data`
#'
#' @param proj `fastan::project`.
#'
#' @return real value list.
#'
#' @import distributional
#' @import purrr
real_from_prior = function(proj) {
  stopifnot("project must have prior" = !is.null(proj$prior))
  prior = proj$prior
  n.fac = length(prior$alpha$mean)
  nrow = length(prior$alpha$mean[[1]])

  alpha =
    dist_multivariate_normal(prior$alpha$mean, prior$alpha$cov) |>
    generate(1) |>
    {\(.) do.call(rbind, .)}() |>
    t()
  lambda =
    dist_multivariate_normal(prior$lambda$mean, prior$lambda$cov) |>
    generate(1) |>
    {\(.) do.call(rbind, .)}()
  sigma2 =
    dist_gamma(shape = prior$sigma2$shape, rate = prior$sigma2$rate) |>
    generate(nrow) |>
    purrr::pluck(1) |>
    as.matrix()

  real = list(
    alpha  = alpha * proj$prior$alpha$in_group,
    lambda = lambda,
    sigma2 = sigma2,
    group.sizes = proj$data$dim$group.sizes
  )

  real
}


#' Generate real value list from posterior
#'
#' Used on `fastan::generate_data`
#'
#' @param proj `fastan::project`.
#' @param stat string with pontual Bayes estimator on project summary.
#'
#' @return real value list.
#'
#' @export
real_from_posterior = function(proj, stat = "mean") {
  stopifnot("project must have summary" = !is.null(proj$summary))
  smry = proj$summary
  list(
    alpha  = smry$alpha[,,stat, drop=T]  |> as.matrix(),
    lambda = smry$lambda[,,stat, drop=T] |> as.matrix() |> {\(.) if (ncol(.) == 1) t(.) else .}(),
    sigma2 = smry$sigma2[,,stat, drop=T] |> as.matrix(),
    group.sizes = proj$data$dim$group.sizes
  )
}


#' Process data in `fastan::data` format
#'
#' (exploratory, confirmatory or semi-confirmatory models)
#'
#' @param data data.frame in the longer format containing data.
#' @param value string, name of the column containing values.
#' @param row string, name of the column containing rows/loadings.
#' @param col string, name of the column containing columns/factor-levels.
#' @param group string, default NULL considers the model as exploratory (i.e., only one factor), name of the column containing groups.
#'
#' @return `fastan` model object.
#'
#' @export
#'
#' @import dplyr
#' @import purrr
process_data = function(data, value, row, col, group = NULL) {
  if (is.null(group)) {
    data$xxx = 1
    group = "xxx"
  }

  label = list(
    factor_score = unique(data[[col]])  ,
    group        = unique(data[[group]]),
    loading      = unique(data[[row]])
  )

  data_fa =
    data |>
    dplyr::rename("value" = !!value,
                  "row"   = !!row,
                  "col"   = !!col,
                  "group" = !!group,
                  ) |>
    {\(.) .[c("value", "row", "col", "group")]}() |>
    {\(.)
    dplyr::mutate(.,
      row    = .$row   |> factor(label$loading)      |> as.numeric(),
      col    = .$col   |> factor(label$factor_score) |> as.numeric(),
      group  = .$group |> factor(label$group)        |> as.numeric()
    )}()

  group.sizes =
    data_fa |>
    dplyr::select(dplyr::all_of(c("row", "group"))) |>
    unique() |>
    dplyr::group_by_at("group") |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::select(!dplyr::all_of("row")) |>
    unique() |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of("n")) |>
    purrr::pluck(1)

  data =
    list(
      obs = data_fa,
      dim = list(row = max(data_fa$row),
                 col = max(data_fa$col),
                 group.sizes = group.sizes
                 ),
      label = label
  )

  if(any(is.na(data$obs$value))) {
    data$pred =
      data$obs |>
      {\(.) dplyr::filter(., is.na(.$value))}()

    data$obs =
      data$obs |>
      {\(.) dplyr::filter(., !is.na(.$value))}()
  }

  to_data(data)
}


#' Generate binary matrix to indicate where are the loadings related to groups.
#'
#' The loadings related to groups are the non-zero loadings a priori.
#'
#' @param group.sizes integer vector with size of each group.
#' @param semi.conf logical, `TRUE` if the model is semi-confirmatory, `FALSE` otherwise.
#'
#' @return binary matrix.
#'
#' @export
alpha_in_group = function(group.sizes, semi.conf) {
  group_limits = group_limits(group.sizes)
  n.fac = length(group.sizes) - as.numeric(semi.conf)
  alpha = matrix(0, nrow = sum(group.sizes), ncol = n.fac)
  for (i in 1:n.fac) {
    alpha[group_limits[[1]][i] : group_limits[[2]][i], i] = rep(1, group.sizes[i])
  }
  if (semi.conf) {
    i = i +1
    alpha[group_limits[[1]][i] : group_limits[[2]][i],] = 1
  }
  alpha
}
