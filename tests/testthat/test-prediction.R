test_that("get_nearest_row works", {
  df = data.frame(id = 1:7, lat = rep(0, 7), lon = 1:7)
  id = c(1,3,7)
  expect_equal(
    get_nearest_row(small = df[id,], big = df[-id,], "id") |> as.matrix() |> round() |> `dimnames<-`(NULL),
    matrix(c(1,3,7,  2,4,6,  111,111,111), ncol = 3)
  )

  df = data.frame(id = 1:9, lat = rep(1:3, times = 3), lon = rep(1:3, each = 3))
  id = c(1,2,7,9)
  expect_equal(
    get_nearest_row(small = df[id,], big = df[-id,], "id") |> as.matrix() |> round() |> `dimnames<-`(NULL),
    matrix(c(1,2,7,9,  4,3,8,6,  111,111,111,111), ncol = 3)
  )
})


test_that("missing_validation_selection works", {
  df = data.frame(
    value = 101:124,
    row = rep(1:8, each = 3),
    col = rep(1:3, times = 8),
    group = 1
  )
  df[c(4,7,17,24),1] = NA

  proj1 =
    project() |>
    set_data(data = df, value = "value", row = "row", col = "col", group = "group") |>
    #set_data(simdata = T, seed = 1234, group.sizes = 8, columns = 5, pred = .12) |>
    set_space(df = data.frame(id = 1:8, lat = rep(0,8), lon = 1:8, alt = rep(0,8)), label = "id", lat = "lat", lon = "lon", alt = "alt")
  proj2 = proj1
  proj2$data = missing_validation_selection(proj1)

  expect_equal(  # correct association between points, i.e., nearest point wo missing
    c( proj1$data$pred$row, proj2$data$label$loading[proj2$data$pred$row] ) |>
      matrix(ncol=2),
    matrix(c(2,3,6,8,  1,4,5,7), ncol=2)
  )

  expect_equal(  # no mess in columns
    proj1$data$pred$col,
    proj2$data$pred$col
  )

  expect_equal(  # true values correctly selected
    proj2$data$pred$value,
    c(101, 110, 114, 121)
  )
})
