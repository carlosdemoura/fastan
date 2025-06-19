test_that("fiat_groups_limits() works", {
  ans =
    list(
      c(1, 2, 4),
      c(1, 3, 6)
    )
  expect_equal(fiat_groups_limits(1:3), ans)
})
