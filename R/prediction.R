#' Use LSAP algorithm to select nearest points
#'
#' @param small data.frame (with rows without missings).
#' @param big data.frame (with rows with missings).
#' @param id string with id column.
#' @param lon string with longitude column.
#' @param lat string with latitude column.
#'
#' @return data.frame with relations by id.
#'
#' @importFrom clue solve_LSAP
#' @import dplyr
#' @importFrom geosphere distm
#' @import purrr
get_nearest_row = function(small, big, id, lon = "lon", lat = "lat") {
  dist_mat =
    geosphere::distm(
      x = small[, c(lon, lat)],
      y = big[, c(lon, lat)]
    )

  assignment = clue::solve_LSAP(dist_mat)

  data.frame(
    row_small = purrr::pluck(as.vector(small[id]),1),
    row_big = purrr::pluck(as.vector(big[id]),1)[assignment],
    dist = dist_mat[cbind(1:nrow(small), assignment)]/1000
  )
}


#' Missing validation algorithm
#'
#' @param proj `fastan::project` object.
#'
#' @return `fastan::data` object.
#'
#' @import dplyr
#' @import purrr
missing_validation_selection = function (proj) {
  pred_new = proj$data$pred[NULL,]
  for (group_ in 1:length(proj$data$label$group)) {
    rows_group   = dplyr::filter(proj$data$obs, proj$data$obs$group == group_) |> {\(.) .$row}() |> unique()
    rows_missing = dplyr::filter(proj$data$pred, proj$data$pred$group == group_) |> {\(.) .$row}() |> unique()
    rows_present = setdiff(rows_group, rows_missing)

    small =
      proj$space |>
      {\(.) dplyr::filter(., .$row %in% rows_missing)}()

    big =
      proj$space |>
      {\(.) dplyr::filter(., .$row %in% rows_present)}()

    result = get_nearest_row(small, big, "row")

    df_x =
      proj$data$obs |>
      {\(.) dplyr::filter(., .$row %in% rows_present) }() |>
      dplyr::mutate(
        row = proj$data$label$loading[.data$row],
        col = proj$data$label$factor_score[.data$col],
        group = proj$data$label$group[.data$group]
      )

    df_pred =
      proj$data$pred |>
      {\(.) dplyr::filter(., .$row %in% rows_missing)}() |>
      dplyr::left_join(data.frame(row = result$row_small, nearest_row = result$row_big), by = "row") |>
      dplyr::select(dplyr::all_of(c("value", "nearest_row", "col", "group"))) |>
      `colnames<-`(c("value", "row", "col", "group")) |>
      dplyr::mutate(
        row   = proj$data$label$loading[.data$row],
        col   = proj$data$label$factor_score[.data$col],
        group = proj$data$label$group[.data$group]
      )

    df_pred_real_value =
      dplyr::left_join(df_pred[,c("row", "col")], df_x, by = c("row", "col")) |>
      dplyr::select(dplyr::all_of(c("value", "row", "col", "group")))

    pred_new = rbind(pred_new, df_pred_real_value)
  }

  data_new =
    proj$data$obs |>
    {\(.) dplyr::filter(., !(.$row %in% proj$data$pred$row)) }() |>
    dplyr::mutate(
      row = proj$data$label$loading[.data$row],
      col = proj$data$label$factor_score[.data$col],
      group = proj$data$label$group[.data$group]
    ) |>
    process_data("value", "row", "col", "group")

  data_new$pred =
    pred_new |>
    {\(.)
      dplyr::mutate(.,
                    row   = lapply(.$row,   function(x){which(data_new$label$loading == x,      data_new$label$loading)})      |> unlist(),
                    col   = lapply(.$col,   function(x){which(data_new$label$factor_score == x, data_new$label$factor_score)}) |> unlist(),
                    group = lapply(.$group, function(x){which(data_new$label$group == x,        data_new$label$group)})        |> unlist()
      )
    }() |>
    dplyr::arrange(.data$row, .data$col)

  data_new$obs = dplyr::anti_join(data_new$obs, data_new$pred, by=c("row", "col"))

  data_new
}
