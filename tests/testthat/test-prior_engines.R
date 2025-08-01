test_that("car works", {
  expect_error(car(matrix(c(0,1,0,  1,0,1,  0,1,0), nrow=3), tau = 1))
  expect_equal(car(matrix(1)), matrix(20))
})


test_that("car_engine works", {  # could d be better
  expect_error(car_engine(type = "xxx", NA, NA, NA))
})
