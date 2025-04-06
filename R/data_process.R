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
#' @import stats
#' @import utils
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
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = stats::runif(rows.by.group[i], -5, 5)

    lambda[i, ] = stats::rnorm(columns, 0, 1) |> sort(decreasing = as.logical(i %/% 2))
  }

  if (semi.conf) {
    i = i + 1
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], ] =
      matrix(
        stats::runif(rows.by.group[i] * n.fac, -5, 5),
        nrow = rows.by.group[i],
        ncol = n.fac
      )
  }

  sigma2 = stats::runif(sum(rows.by.group), .5, 5)

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
#'
#' @return `fastan` model object
#'
#' @export
#'
#' @import dplyr
#' @import utils
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

  list(
    data = data_fa,
    dim = list(al_row  = max(data_fa$row),
               al_col  = max(data_fa$col),
               al_fac  = max(data_fa$group) - as.numeric(semi.conf)
               ),
    var_alpha_prior = var_alpha_prior,
    labels = labels
  )
}


#' Process data in `fastan` model format (cluster model)
model_data_cluster = function() {

}
