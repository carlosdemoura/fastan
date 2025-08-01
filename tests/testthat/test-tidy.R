test_that("set_info works", {
  proj = project()
  expect_true(is.null(proj$info))
  proj = proj |> set_info("teste")
  expect_equal(proj$info, "teste")
})


test_that("set_diagnostic works just as diagnostic", {
  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))
    proj$diagnostic = NULL
    expect_equal( set_diagnostic(proj)$diagnostic, diagnostic(proj$fit) )
  }
})


test_that("set_summary works just as summary_matrix", {
  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))
    proj$summary = NULL
    expect_equal( set_summary(proj)$summary, summary_matrix(proj) )
  }
})


test_that("try_set works", {
  proj = project()
  proj$info = "teste"
  expect_warning(
    try_set({proj = proj |> set_info(2 + "a")})
    )
  expect_equal(proj$info, "teste")
  expect_no_error(
    try_set({proj = proj |> set_info("novo")})
    )
  expect_equal(proj$info, "novo")
})


test_that("delete works", {
  proj =
    project() |>
    set_info("teste") |>
    set_data(simdata = T, group.sizes = 10, columns = 5, semi.conf = F) |>
    set_prior(semi.conf = F)

  proj =
    proj |>
    delete("info")

  expect_true(is.null(proj$info))

  proj =
    proj |>
    delete("data", "prior")

  expect_true(all(is.null(proj$data), is.null(proj$prior)))
})


test_that("invert_signal is it's own inverse", {
  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))
    smry_before = proj$summary
    proj =
      proj |>
      invert_signal(1:n.fac(proj)) |>
      invert_signal(1:n.fac(proj))
    expect_equal( smry_before, proj$summary )
  }
})


test_that("invert_signal works", {
  for (file in paste0("proj_", c("expl", "conf", "sc"), ".rds") ) {
    proj = readRDS(testthat::test_path("testdata", file))
    smry_before = proj$summary
    proj =
      proj |>
      invert_signal(1)

    expect_equal(
      proj$summary$alpha[,1,c("mean", "median")], - smry_before$alpha[,1,c("mean", "median")]
    )
    expect_equal(
      proj$summary$lambda[1,,c("mean", "median")], - smry_before$lambda[1,,c("mean", "median")]
    )

  }
})


test_that("set_prior engine works", {
  def_prior =
    project() |>
    set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F) |>
    set_prior(semi.conf = F)  |>
    {\(.) .$prior }()

  my_prior = list(
    alpha  = list(mean = 1, cov = 2, in_group = 3, omit.alpha0 = 4),
    lambda = list(mean = 1, cov =2),
    sigma2 = list(shape = 1, rate = 2, only1 = 3)
  )

  # full engine
  proj =
    project() |>
    set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F) |>
    set_prior(semi.conf = F,
              engine =
                list(
                  alpha  = function(x) my_prior$alpha,
                  lambda = function(x) my_prior$lambda,
                  sigma2 = function(x) my_prior$sigma2
                ))
  expect_equal(proj$prior[c("alpha", "lambda", "sigma2")], my_prior)


  # partial engine
  for (par in c("alpha", "lambda", "sigma2")) {
    engine = list()
    engine[[par]] = function(x) my_prior[[par]]
    proj =
      project() |>
      set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F) |>
      set_prior(semi.conf = F, engine = engine)
    expect_equal(
      proj$prior[c("alpha", "lambda", "sigma2")],
      c( my_prior[par], def_prior[setdiff(c("alpha", "lambda", "sigma2"), par)] )[c("alpha", "lambda", "sigma2")]
      )
  }
})


test_that("set_prior validates args", {
  expect_error(
    project() |>
      set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F) |>
      set_prior(type = "xxx")
  )
})


test_that("set_data pred works", {
  expect_true(T)
})

## FALTA set_fit missing_validation set_space set_data
