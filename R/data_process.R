#' Generate data (confirmatory or semi-confirmatory models)
#'
#' @param real .
#' @param cicles .
#'
#' @return fastan model object
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import stats
#' @import tidyr
#' @import utils
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

  epsilon = matrix(
    stats::rnorm(sum(group.sizes)*columns*cicles, 0, sqrt(real$sigma2)) ,
    ncol = columns*cicles,
    byrow = F
  )

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


#' Title
#'
#' @param group.sizes .
#' @param columns .
#' @param semi.conf .
#' @param param .
#'
#' @import stats
real_from_uniform = function(group.sizes, columns, semi.conf, param = list(alpha = c(1,6), lambda = c(.5,1.5), sigma2 = c(1,3))) {
  stopifnot(
    "if the model is semi.conf there
    must be at least three groups" = ifelse(semi.conf, length(group.sizes) >= 3, T)
  )

  normalize = function(x) {
    param$lambda[1] + (x - min(x)) / (max(x) - min(x)) * (param$lambda[2] - param$lambda[1])
  }

  n.fac = length(group.sizes) - as.integer(semi.conf)
  groups_limits = fiat_groups_limits(group.sizes)

  alpha = matrix(0,
                 nrow = sum(group.sizes),
                 ncol = n.fac)

  lambda = matrix(0,
                  ncol = columns,
                  nrow = n.fac)

  for (i in 1:n.fac) {
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = stats::runif(group.sizes[i], param$alpha[1], param$alpha[2])
    for (j in 2:columns) {
      lambda[i,j] = rnorm(1, lambda[i,j-1])
    }
  }

  lambda =
    lambda |>
    apply(1, normalize) |>
    t()

  if (semi.conf) {
    i = i + 1
    mat1 =
      matrix(
        rep(stats::runif(group.sizes[i], param$alpha[1], param$alpha[2]), n.fac),
        nrow = group.sizes[i],
        ncol = n.fac
      )
    mat2 =
      replicate(
        group.sizes[i],
        stats::rbeta(n.fac, 0.1, 0.2) |> {\(.) ./sum(.)}()
      ) |> t()
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], ] = mat1 * mat2 |> {\(.) ifelse(. < 1e-2, 0, .)}()
  }

  sigma2 = stats::runif(sum(group.sizes), param$sigma2[1], param$sigma2[2]) |> as.matrix()

  list(alpha = alpha, lambda = lambda, sigma2 = sigma2, group.sizes = group.sizes)
}


#' Title
#'
#' @param proj .
#' @param stat .
real_from_posterior = function(proj, stat = "mean") {
  stopifnot("project must have summary" = !is.null(proj$summary))
  smry = proj$summary
  list(
    alpha  = smry$alpha[,,stat, drop=F],
    lambda = smry$lambda[,,stat, drop=F],
    sigma2 = smry$sigma2[,,stat, drop=F],
    rows.by.broup = proj$data$dim$group.sizes
  )
}


#' Title
#'
#' @param proj .
#'
#' @import MASS
#' @import stats
real_from_prior = function(proj) {
  stopifnot("project must have prior" = !is.null(proj$prior))
  prior = proj$prior
  n.fac = length(prior$alpha$mean)
  nrow = length(prior$alpha$mean[[1]])

  alpha = matrix(0,
                 nrow = nrow,
                 ncol = n.fac)
  lambda = matrix(0,
                  ncol = length(prior$lambda$mean[[1]]),
                  nrow = n.fac)

  for (i in 1:n.fac) {
    alpha[,i]  = MASS::mvrnorm(1, prior$alpha$mean[[i]],  prior$alpha$cov[[i]])
    lambda[i,] = MASS::mvrnorm(1, prior$lambda$mean[[i]], prior$lambda$cov[[i]])
  }

  list(
    alpha  = alpha,
    lambda = lambda,
    sigma2 = stats::rgamma(nrow, shape = prior$sigma2$shape, scale = prior$sigma2$scale) |> as.matrix(),
    group.sizes = proj$data$dim$group.sizes
  )
}


#' Process data in `fastan` model format (confirmatory or semi-confirmatory models)
#'
#' @param data data.frame in the longer format. ### obs já arranjado já com grupos como fatores, já com último fator como grupo semi.conf se for o caso
#' @param value string; name of the column containing values.
#' @param group string; name of the column containing groups.
#' @param row string; name of the column containing rows/loadings.
#' @param col string; name of the column containing columns/factor-levels.
#'
#' @return `fastan` model object
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import utils
process_data = function(data, value, row, col, group = NULL) {
  if (is.null(group)) {
    data$xxx = 1
    group = "xxx"
  }

  labels = list(
    factor_level = unique(data[[col]])  ,
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
      row    = .$row   |> factor(labels$loading)      |> as.numeric(),
      col    = .$col   |> factor(labels$factor_level) |> as.numeric(),
      group  = .$group |> factor(labels$group)        |> as.numeric()
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
      x = data_fa,
      dim = list(row = max(data_fa$row),
                 col = max(data_fa$col),
                 group.n = max(data_fa$group),
                 group.sizes = group.sizes),
      labels = labels
  )

  if(any(is.na(data$x$value))) {
    data$pred =
      data$x |>
      {\(.) dplyr::filter(., is.na(.$value))}()

    data$x =
      data$x |>
      {\(.) dplyr::filter(., !is.na(.$value))}()
  }

  data
}
