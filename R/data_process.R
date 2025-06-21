#' Generate data (confirmatory or semi-confirmatory models)
#'
#' @param rows.by.group integer vector; size (number of loadings - or rows in the wider format) of each group.
#' @param columns integer; number of columns in the wider format.
#' @param cicles integer; UNDER DEVELOPMENT.
#' @param semi.conf boolean:
#' @param real .
#' @param pred .
#'
#' @return fastan model object
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stats
#' @import utils
#' @import purrr
generate_data = function(rows.by.group, columns, cicles = 1, semi.conf = F, real = list(alpha = c(1,6), lambda = c(.5,1.5), sigma2 = c(1,5)), pred = 0) {
  normalize = function(x) {
    real$lambda[1] + (x - min(x)) / (max(x) - min(x)) * (real$lambda[2] - real$lambda[1])
  }

  #rows.by.group = rep(10, 3); columns = 8; cicles = 1; semi.conf = T
  stopifnot(
    "if the model is semi.conf there
    must be at least three groups" = ifelse(semi.conf, length(rows.by.group) >= 3, T),
    "it's necessary 0 <= pred < 1 " = (0 <= pred) & (pred < 1)
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
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = stats::runif(rows.by.group[i], real$alpha[1], real$alpha[2])
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
        rep(stats::runif(rows.by.group[i], real$alpha[1], real$alpha[2]), n.fac),
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

  sigma2 = stats::runif(sum(rows.by.group), real$sigma2[1], real$sigma2[2])

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

  data = process_data(x, "value", "group", "row", "col")
  data$real = list(alpha = alpha,
                   lambda = lambda,
                   sigma2 = as.matrix(sigma2)
                   )
  if (pred) {
    index = 1:nrow(data$x) |> sample(size = floor(pred * nrow(data$x) * 0.9), replace = F) |> sort()
    data$pred = data$x[index, ]
    data$x = data$x[-index, ]
  }
  return(data)
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
#' @import utils
#' @import purrr
process_data = function(data, value, group, row, col) {
  #data = x; row = "row"; group = "group"; col = "col"; value = "value"
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


#' Title
#'
#' @param proj .
#' @param ... .
#'
#' @export
#'
#' @import dplyr
generate_data_from_project = function(proj, ...) {
  data = generate_data(rows.by.group = proj$data$dim$group.sizes, columns = proj$data$dim$col, ...)
  if (is.null(proj$data$pred)) {
    return(data)
  } else {
    data$pred = dplyr::left_join(proj$data$pred[c("row", "col")], data$x, by = c("row", "col"))  |> dplyr::relocate(dplyr::all_of("value"))
    data$x = dplyr::right_join(data$x, proj$data$x[c("row", "col")], by = c("row", "col"))
    return(data)
  }
}


#' Title
#'
#' @param data .
#'
#' @export
prop.missing = function(data) {
  data = validate_proj_arg(data, "data")
  ifelse(!is.null(data$pred), nrow(data$pred), 0) / nrow(data$x)
}
