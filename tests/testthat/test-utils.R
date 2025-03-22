test_that("fiat_sentinel() return sentinel values", {
  expect_equal(fiat_sentinel(matrix(1:4, nrow = 2)), 1000)
})

test_that("fiat_sentinel() validates argument", {
  expect_error(fiat_sentinel("1"))
})
