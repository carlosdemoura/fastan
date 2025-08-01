test_that("distances works", {
  m1 = distances(data.frame(lon = c(0,0), lat = c(-1,1))) |> round()
  m2 = matrix(c(0, 221, 221, 0), nrow = 2)
  expect_equal(m1, m2)

  m1 = distances(data.frame(lon = c(0,0), lat = c(-1,1)), pairs = T) |> round() |> as.matrix() |> `dimnames<-`(NULL)
  m2 = matrix(c(1,1,2,2,  1,2,1,2,  0, 221, 221, 0), nrow = 4)
  expect_equal(m1, m2)
})


test_that("neib_dist works", {
  expect_warning(neib_dist(data.frame(lon = c(0,0), lat = c(-1,1))))
  expect_equal(
    neib_dist(data.frame(lon = c(0,0), lat = c(-1,1)), dist = 1),
    matrix(0, nrow=2, ncol=2)
  )
  expect_equal(
    neib_dist(data.frame(lon = c(0,0), lat = c(-1,1)), 300),
    matrix(c(0,1,1,0), nrow=2, ncol=2)
  )
  expect_equal(
    neib_dist(data.frame(lon = c(0,0,0), lat = c(-1,0,1)), 150),
    matrix(c(0,1,0,  1,0,1,  0,1,0), nrow=3)
  )
})


test_that("neib_simple works", {
  expect_equal(
    neib_simple(3),
    matrix(c(0,1,0,  1,0,1,  0,1,0), nrow=3)
  )

  expect_equal(
    neib_simple(4),
    matrix(c(0,1,0,0,  1,0,1,0,  0,1,0,1,  0,0,1,0), nrow=4)
  )
})


test_that("neib_voronoi works", {  # could be better
  expect_equal(
    neib_voronoi(data.frame(lon = c(0,1,0), lat = c(-1,0,1))),
    matrix(1, 3, 3) - diag(rep(1, 3))
  )
})

## FALTA space_process generate_space
