#' Title
#'
#' @param data .
#' @param df .
#' @param label .
#' @param lat .
#' @param lon .
#' @param alt .
#' @param position .
#'
#' @export
#'
#' @import dplyr
space_process = function(data, df, label, lat, lon, alt, position = "row") {
  stopifnot("position must be row or col" = position %in% c("row", "col"))
  data = validate_proj_arg(data, "data")
  {\(.)
    if (position == "row") data.frame(id = data$label$loading     )
    else                   data.frame(id = data$label$factor_level)
    }() |>
  {\(.) dplyr::mutate(., loc = 1:nrow(.))}() |>
  `colnames<-`(c("id", position)) |>
  dplyr::left_join(
    data.frame(id = df[[label]], lon = df[[lon]], lat = df[[lat]], alt = df[[alt]]),
    by = "id"
  ) |>
  dplyr::as_tibble()
}


#' Title
#'
#' @param n .
#' @param cont .
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom geosphere randomCoordinates
#' @importFrom stats runif
generate_space = function(n, cont = F) {
  geosphere::randomCoordinates(n) |>
    as.data.frame() |>
    {\(.) if (cont) dplyr::arrange(., .$lon) else .}() |>
    cbind(data.frame(alt = round(runif(n, 0, 100), 2)))
}


#' Title
#'
#' @param coor .
#' @param lon .
#' @param lat .
#' @param pairs .
#'
#' @export
#'
#' @import dplyr
#' @importFrom geosphere distm
#' @importFrom tidyr pivot_longer
distances = function(coor, lon = "lon", lat = "lat", pairs = F) {
  dist =
    as.matrix(coor[, c(lon, lat)]) |>
    geosphere::distm() |>
    {\(.) ./1000}()

  if (pairs) {
    n = ncol(dist)
    dist =
      dist |>
      as.data.frame() |>
      `colnames<-`(1:n) |>
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'est2', values_to = 'dist') |>
      dplyr::mutate(
        est1 = rep(1:n, each = n),
        est2 = as.integer(.data$est2)
      ) |>
      dplyr::relocate(dplyr::all_of("est1"))
  }

  return(dist)
}


#' Title
#'
#' @param coor .
#' @param dist in km
#' @param lon .
#' @param lat .
#'
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything
neib_dist = function(coor, dist = NULL, lon = "lon", lat = "lat") {
  if (is.null(dist)) {
    temp =
      distances(coor, pairs = T) |>
      {\(.) .[.$dist>0,]}() |>
      group_by(.data$est1) |>
      summarise(
        dist = min(.data$dist)
      )
    dist = max(temp$dist) + 0.01
    warning(paste0("minimum distance considered: ", dist))
  }

  distances(coor, lon, lat) |>
    {\(.) 1*(. <= dist)}() |>
    {\(.) . - diag(diag(.))}()
}


#' Title
#'
#' @param coor .
#' @param lon .
#' @param lat .
#'
#' @export
#'
#' @import deldir
neib_voronoi = function(coor, lon = "lon", lat = "lat") {
  voronoi = deldir::deldir(coor[[lon]], coor[[lat]])

  n = nrow(coor)
  mat = matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    neib = deldir::getNbrs(voronoi)[[i]]
    mat[i, neib] = 1
  }

  mat
}


#' Title
#'
#' @param n .
neib_simple = function(n) {
  W = matrix(0, nrow = n, ncol = n)
  W[1,2] = 1
  W[n,n-1] = 1
  for (i in 2:(n-1)) {
    W[i, c(i-1, i+1)] = 1
  }
  W
}
