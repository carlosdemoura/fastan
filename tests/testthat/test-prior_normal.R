test_that("alpha_cov_to_var works", {
  mat1 =
    project() |>
    set_data(simdata = T, group.sizes = 3, columns = 5, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    {\(.) alpha_cov_to_var(.$prior)}()
  mat2 = rep(10, 3) |> as.matrix()
  expect_equal(mat1, mat2)


  mat1 =
    project() |>
    set_data(simdata = T, group.sizes = rep(2,3), columns = 5, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    {\(.) alpha_cov_to_var(.$prior)}()
  mat2 = matrix(c(
    10.0, 10.0,  0.1,  0.1,  0.1,  0.1,
    0.1,  0.1, 10.0, 10.0,  0.1,  0.1,
    0.1,  0.1,  0.1,  0.1, 10.0, 10.0
  ), ncol = 3)
  expect_equal(mat1, mat2)


  mat1 =
    project() |>
    set_data(simdata = T, group.sizes = rep(2,3), columns = 5, semi.conf = T) |>
    set_prior(semi.conf = T) |>
    {\(.) alpha_cov_to_var(.$prior)}()
  mat2 = matrix(c(
    10.0, 10.0,  0.1,  0.1, 10.0, 10.0,
    0.1,  0.1, 10.0, 10.0, 10.0, 10.0
  ), ncol = 2)
  expect_equal(mat1, mat2)
})
