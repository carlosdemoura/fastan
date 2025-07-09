
car_alpha = function(neib, cov, type=1) {
  car = car(neib)
  vals = cov |> c() |> unique() |> sort()
  min = vals[1]; med = vals[2]; max = vals[3]
  d_cov_med = which(diag(cov) == med, diag(cov))
  car[d_cov_med,] = 0
  car[,d_cov_med] = 0
  car = max * car

  if (type==1) {
    car + diag(diag(cov))

  } else if (type ==2) {
    diag_ = diag(cov)
    diag_[diag_==10]=0
    car + diag_

  } else if (type == 3){
    diag(car) = .01
    cov + car

  }
}

neib = neib_voronoi(proj$space)
cov = proj$prior$alpha$cov[[1]]


# # original
#
# car_voronoi = function(space, x) {
#   car = car(neib_voronoi(space))
#   x[x==.01]=0
#   d10 = which(diag(x)==10, diag(x))
#   d0  = which(diag(x)!=10, diag(x))
#   car[d0,] = 0
#   car[,d0] = 0
#   car = 10 * car
#   diag(car) = .01
#   x + car
# }
