#' Generate data semi.conf
#'
#' @param rows.by.group integer vector
#' @param columns integer
#' @param cicles integer
#' @param semi.conf boolean
#'
#' @return
#'
#' @examples
#' generate_data_sc(rows.by.group = rep(10, 2), columns = 8, cicles = 2, semi.conf = T)
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
generate_data_sc = function(rows.by.group, columns, cicles = 1, semi.conf = F) {
  #rows.by.group = rep(10, 3); columns = 8; cicles = 2; semi.conf = T
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
        as.factor(),
      group = as.factor(.$group),
      row   = as.factor(.$row)
    )}()

  data = process_conf(x, "value", "group", "row", "col")
  data$real = list(alpha = alpha,
                   lambda = lambda,
                   sigma2 = sigma2
                   )
  return(data)
}
