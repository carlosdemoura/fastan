rows_missing = proj$data$pred$row |> unique()
rows_present = setdiff(proj$data$x$row |> unique(), rows_missing)

df_missing =
  proj$space |>
  {\(.) dplyr::filter(., .$row %in% rows_missing)}()

df_present =
  proj$space |>
  {\(.) dplyr::filter(., .$row %in% rows_present)}()


# Distância (Haversine) entre cada missing e cada present
dist_matrix =
  geosphere::distm(
    x = df_missing[, c("lon", "lat")],
    y = df_present[, c("lon", "lat")],
    fun = geosphere::distHaversine
    )

# Resolver o problema de assignment ótimo
assignment = clue::solve_LSAP(dist_matrix)

# Juntar os pares correspondentes
result =
  df_missing |>
  dplyr::mutate(
    nearest_row = df_present$row[assignment],
    nearest_dist = dist_matrix[cbind(1:nrow(df_missing), assignment)]
  )


df =
  proj$data$x |>
  {\(.) dplyr::filter(., .$row %in% result$nearest_row) }()


df =
  df |>
  dplyr::mutate(
    row = proj$data$label$loading[row],
    col = proj$data$label$factor_level[col],
    group = proj$data$label$group[group]
  )

df_pred =
  proj$data$pred |>
  dplyr::left_join(data.frame(row = result$row, nearest_row = result$nearest_row), by = "row") |>
  dplyr::select(dplyr::all_of(c("value", "nearest_row", "col", "group"))) |>
  `colnames<-`(c("value", "row", "col", "group"))


data =
  proj$data$x |>
  {\(.) dplyr::filter(., .$row %in% result$nearest_row) }() |>
  dplyr::mutate(
    row = proj$data$label$loading[row],
    col = proj$data$label$factor_level[col],
    group = proj$data$label$group[group]
  ) |>
  process_data("value", "row", "col", "group")

df_pred =
  proj$data$pred |>
  dplyr::left_join(data.frame(row = result$row, nearest_row = result$nearest_row), by = "row") |>
  dplyr::select(dplyr::all_of(c("value", "nearest_row", "col", "group"))) |>
  `colnames<-`(c("value", "row", "col", "group")) |>
  dplyr::mutate(row=as.factor(row) |> as.numeric())

data$pred = dplyr::right_join(data$x, df_pred[,c("row", "col")], by=c("row", "col"))
data$x = dplyr::anti_join(data$x, data$pred, by=c("row", "col"))

data
