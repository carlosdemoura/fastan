test_that("generate_data works", {
  real = list(alpha = as.matrix(rep(2,4)), lambda = t(as.matrix(rep(10,4))), sigma2 = as.matrix(rep(0,4)), group.sizes = 4)
  expect_equal(generate_data(real), readRDS(testthat::test_path("testdata", "generate_data.rds")))

  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))
    real = proj$data$real[c("alpha", "lambda", "sigma2")]
    real$group.sizes = proj$data$dim$group.sizes
    set.seed(12345)
    expect_equal(generate_data(real)$obs[,2:4], proj$data$obs[,2:4])
  }
})


test_that("real_from_prior generate data in correct dimension", {
  real_from_dist(group.sizes = 10, columns = 5, semi.conf = F)[c("alpha", "lambda", "sigma2")] |>
    lapply(dim) |>
    expect_equal(list(alpha = c(10, 1), lambda = c(1, 5), sigma2 = c(10, 1)))

  real_from_dist(group.sizes = rep(10, 3), columns = 5, semi.conf = F)[c("alpha", "lambda", "sigma2")] |>
    lapply(dim) |>
    expect_equal(list(alpha = c(30, 3), lambda = c(3, 5), sigma2 = c(30, 1)))

  real_from_dist(group.sizes = rep(10, 3), columns = 5, semi.conf = T)[c("alpha", "lambda", "sigma2")] |>
    lapply(dim) |>
    expect_equal(list(alpha = c(30, 2), lambda = c(2, 5), sigma2 = c(30, 1)))
})


test_that("real_from_prior generate data in correct dimension", {
  project() |>
    set_data(simdata = T, group.sizes = 10, columns = 5, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    real_from_prior() |>
    {\(.) .[c("alpha", "lambda", "sigma2")]}() |>
    lapply(dim) |>
    expect_equal(list(alpha = c(10, 1), lambda = c(1, 5), sigma2 = c(10, 1)))

  project() |>
    set_data(simdata = T, group.sizes = rep(10, 3), columns = 5, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    real_from_prior() |>
    {\(.) .[c("alpha", "lambda", "sigma2")]}() |>
    lapply(dim) |>
    expect_equal(list(alpha = c(30, 3), lambda = c(3, 5), sigma2 = c(30, 1)))

  project() |>
    set_data(simdata = T, group.sizes = rep(10, 3), columns = 5, semi.conf = T) |>
    set_prior(semi.conf = T) |>
    real_from_prior() |>
    {\(.) .[c("alpha", "lambda", "sigma2")]}() |>
    lapply(dim) |>
    expect_equal(list(alpha = c(30, 2), lambda = c(2, 5), sigma2 = c(30, 1)))
})


test_that("real_from_posterior generate data in correct dimension", {
  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))

    proj |>
      real_from_posterior() |>
      {\(.) .[c("alpha", "lambda", "sigma2")]}() |>
      lapply(dim) |>
      expect_equal(proj$data$real[c("alpha", "lambda", "sigma2")] |> lapply(dim))
  }
})


test_that("alpha_in_group works", {
  expect_equal(alpha_in_group(10, F), matrix(1, nrow = 10, ncol = 1))
})

### FALTA process_data
