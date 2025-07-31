test_that("group_limits works", {
  ans =
    list(
      c(1, 2, 4),
      c(1, 3, 6)
    )
  expect_equal(group_limits(1:3), ans)
  expect_equal(group_limits(10), list(1,10))
})


test_that("param.dim throws error if proj has no summary or prior", {
  proj =
    new_project() |>
    set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F)

  expect_error(param.dim(proj))
})


test_that("param.dim has the same output if proj have or not summary", {
  proj = readRDS(testthat::test_path("testdata", "proj_expl.rds"))
  p1 = param.dim(proj)
  proj$summary = NULL
  p2 = param.dim(proj)
  expect_equal(p1, p2)
})


test_that("param.dim have an adequate output in different contexts", {
  # exploratory
  new_project() |>
    set_data(simdata = T, group.sizes = 5, columns = 14, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    param.dim() |>
    {\(.) as.matrix(.[1:nrow(.),1:2])}() |>
    `dimnames<-`(NULL) |>
    expect_equal(matrix(c(5,1,1,14,5,1), ncol = 2, byrow = T))

  # confirmatory
  new_project() |>
    set_data(simdata = T, group.sizes = rep(5, 6), columns = 14, semi.conf = F) |>
    set_prior(semi.conf = F) |>
    param.dim() |>
    {\(.) as.matrix(.[1:nrow(.),1:2])}() |>
    `dimnames<-`(NULL) |>
    expect_equal(matrix(c(30,6,6,14,30,1), ncol = 2, byrow = T))

  # semi-confirmatory
  new_project() |>
    set_data(simdata = T, group.sizes = rep(5, 6), columns = 14, semi.conf = T) |>
    set_prior(semi.conf = T) |>
    param.dim() |>
    {\(.) as.matrix(.[1:nrow(.),1:2])}() |>
    `dimnames<-`(NULL) |>
    expect_equal(matrix(c(30,5,5,14,30,1), ncol = 2, byrow = T))
})


test_that("n.fac requires priors", {
  expect_error(n.fac(new_project()))
})


test_that("n.fac works", {
  new_project() |>
    set_data(simdata = T, group.sizes = rep(10, 6), columns = 14, semi.conf = T) |>
    set_prior(semi.conf = T) |>
    n.fac() |>
    expect_equal(5)
})


test_that("prop.missing works", {
  new_project() |>
    set_data(simdata = T, pred = .1, group.sizes = 10, columns = 10, semi.conf = F) |>
    prop.missing() |>
    expect_equal(.1)
})


#### FALTAM: export; elapsed_time_table; loglik; accuracy
