test_that("oop functions return correct class", {
  expect_true(inherits(project(), "project"))
  expect_true(inherits(to_data(list()), "data"))
  expect_true(inherits(to_prior(list()), "prior"))
  expect_true(inherits(to_summary(list()), "summary"))
})


test_that("oop functions interact ok with tidy", {
  proj =
    project() |>
    set_data(simdata = T, group.sizes = 10, columns = 10, semi.conf = F) |>
    set_prior(semi.conf = F)
  expect_true(inherits(proj$data, "data"))
  expect_true(inherits(proj$prior, "prior"))
})

## FALTA o ultimo com summary
