
car_alpha_local = function(proj, tau) {
  cov = list()
  for (fac in 1:n.fac(proj)) {
    lim = proj$data$dim$group.sizes |> fiat_groups_limits()
    rows = lim[[1]][fac]:lim[[2]][fac]
    # if (proj$prior$semi.conf) {
    #   rows =
    #     rows |>
    #     c(lim[[1]][n.fac(proj)]:lim[[2]][n.fac(proj)])
    # }
    coor_group = dplyr::left_join(data.frame(row=rows), proj$space, by = "row")
    neib = neib_voronoi(coor_group)
    car = car(neib) * tau
    m = matrix(0, proj$data$dim$row, proj$data$dim$row)
    m[lim[[1]][fac]:lim[[2]][fac],lim[[1]][fac]:lim[[2]][fac]] = car
    cov[[fac]] = m + diag(.01, proj$data$dim$row)
  }
  cov
}


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

  } else if (type == 4){
    car = car*20

    diag_ = diag(cov)
    diag_[diag_==10]=0
    car + diag(diag_)

  }
}






car_alpha2 = function(neib, cov, type=1) {  # desconsiderando entradas de fora do grupo para calcular car
  car = car(neib)
  vals = cov |> c() |> unique() |> sort()
  min = vals[1]; med = vals[2]; max = vals[3]
  d_cov_med = which(diag(cov) == med, diag(cov))
  car[d_cov_med,] = 0
  car[,d_cov_med] = 0
  car = max * car


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
