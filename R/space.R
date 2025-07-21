#' Process spatial data associated with the project
#'
#' @param data `fastan::data` object.
#' @param df data.frame with spatial data.
#' @param label string, column with id.
#' @param lat string, column with latitude.
#' @param lon string, column with longitude.
#' @param alt string, column with altitude.
#' @param position string, default = "row", if "row" data is associated with loadings, if "col" with factor scores.
#'
#' @return `tibble::tibble`.
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


#' Generate random points
#'
#' @param n integer, number of points to be generated.
#' @param cont logical, default = `FALSE`, if `TRUE` orders the random coordinates by longitude (returning contiguous groups), if `FALSE` does nothing.
#'
#' @return data.frame.
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
    cbind(data.frame(alt = round(stats::runif(n, 0, 100), 2)))
}


#' Get distances (in km) between points
#'
#' (to be used in project space)
#'
#' @param coor data.frame with coordinates.
#' @param lon string, default = "lon", column with longitude.
#' @param lat string, default = "lat", column with latitude.
#' @param pairs logical, default = `FALSE`, if `TRUE` returns data.frame with distances between each pair of point, if `FALSE` returns distance matrix
#'
#' @return matrix/data.frame with distances in km.
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


#' Calculate neighborhood matrix from maximum distance between points
#'
#' @inheritParams distances
#' @param dist numeric, maximum distance (in km) to consider two points as neighbors, default = NULL (calculates the minimum distance that guarantee every point will have at least one neighbor).
#'
#' @return neighborhood matrix.
#'
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything group_by
neib_dist = function(coor, dist = NULL, lon = "lon", lat = "lat") {
  if (is.null(dist)) {
    temp =
      distances(coor, pairs = T) |>
      {\(.) .[.$dist>0,]}() |>
      dplyr::group_by(.data$est1) |>
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


#' Calculate neighborhood matrix from Voronoi tessellation
#'
#' @inheritParams distances
#'
#' @return neighborhood matrix.
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


#' Generate simple neighborhood matrix for line spaces.
#'
#' @param n integer, number of elements in line.
#'
#' @return neighborhood matrix.
#'
#' @export
neib_simple = function(n) {
  W = matrix(0, nrow = n, ncol = n)
  W[1,2] = 1
  W[n,n-1] = 1
  for (i in 2:(n-1)) {
    W[i, c(i-1, i+1)] = 1
  }
  W
}
