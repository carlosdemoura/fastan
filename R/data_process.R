#' Generate data (confirmatory or semi-confirmatory models)
#'
#' @param rows.by.group integer vector; size (number of loadings - or rows in the wider format) of each group.
#' @param columns integer; number of columns in the wider format.
#' @param cicles integer; UNDER DEVELOPMENT.
#' @param semi.conf boolean:
#' @param param list of parameters for generate data:
#' * `TRUE` if it's semi-confirmatory (then the last group is considered to be the semi-confirmatory group);
#' * `FALSE` if it's confirmatory (DEFAULT).
#'
#' @return fastan model object
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stats
#' @import utils
generate_data_sc = function(rows.by.group, columns, cicles = 1, semi.conf = F, param = list(alpha = c(-10,10), lambda = c(.1,2), sigma2 = c(1,5))) {
  normalize = function(x) {
    param$lambda[1] + (x - min(x)) / (max(x) - min(x)) * (param$lambda[2] - param$lambda[1])
  }

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
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = stats::runif(rows.by.group[i], param$alpha[1], param$alpha[2])
    for (j in 2:columns) {
      lambda[i,j] = rnorm(1, lambda[i,j-1])
    }
  }

  lambda =
    lambda |>
    #apply(1, function(x){ x |> scale() |>  {\(.) if(min(.) <= 0) . + abs(min(.)) + .1 else . + .1}()}) |>
    apply(1, normalize) |>
    t()

  if (semi.conf) {
    i = i + 1
    mat1 =
      matrix(
        rep(stats::runif(rows.by.group[i], param$alpha[1], param$alpha[2]), n.fac),
        nrow = rows.by.group[i],
        ncol = n.fac
    )
    mat2 =
      replicate(
        rows.by.group[i],
        stats::rbeta(n.fac, 0.1, 0.2) |> {\(.) ./sum(.)}()
      ) |> t()
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], ] = mat1 * mat2 |> {\(.) ifelse(. < 1e-2, 0, .)}()
  }

  sigma2 = stats::runif(sum(rows.by.group), param$sigma2[1], param$sigma2[2])

  epsilon = matrix(
    stats::rnorm(sum(rows.by.group)*columns*cicles, 0, sqrt(sigma2)) ,
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
          rep(., utils::head(rows.by.group, -1)) |>
            c("group extra" |> rep(utils::tail(rows.by.group, 1)))
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
#' @param semi.conf .
#' @param factor_name .
#' @param normalize .
#'
#' @return `fastan` model object
#'
#' @export
#'
#' @import dplyr
#' @import utils
model_data_sc = function(data, value, group, row, col, semi.conf, factor_name = NULL, normalize = F) {
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
      labels$factor_name = utils::head(labels$factor_name, -1)
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

  mod =
    list(
      data = data_fa,
      dim = list(al_row  = max(data_fa$row),
                 al_col  = max(data_fa$col),
                 al_fac  = max(data_fa$group) - as.numeric(semi.conf)
                 ),
      var_alpha_prior = var_alpha_prior,
      labels = labels
  )

  if(any(is.na(mod$data$value))) {
    mod$pred =
      mod$data |>
      {\(.) dplyr::filter(., is.na(.$value))}()

    mod$data =
      mod$data |>
      {\(.) dplyr::filter(., !is.na(.$value))}()
  }

  if (normalize) {
    mod$data = normalize(mod$data, "value", "row")
  }

  mod
}


#' Normalize df
#' @import dplyr
#' @import rlang
normalize = function(df, value, row) {
  value = rlang::ensym(value)
  row = rlang::ensym(row)

  df |>
    dplyr::group_by(!!row) |>
    dplyr::mutate(
      !!value := (!!value - mean(!!value)) / sd(!!value)
    ) |>
    dplyr::ungroup()
  # df |>
  # dplyr::group_by_at(row) |>
  # dplyr::mutate(
  #   value = (value - mean(value)) / sd(value)
  # ) |>
  # dplyr::ungroup()
}


#' Process data in `fastan` model format (cluster model)
model_data_cluster = function() {

}
