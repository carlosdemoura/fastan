
car_by_groups = function(proj, tau, neib.by.group) {
  cov = list()
  for (fac in 1:n.fac(proj)) {
    lim = proj$data$dim$group.sizes |> fiat_groups_limits()
    rows = lim[[1]][fac]:lim[[2]][fac]
    if (proj$prior$semi.conf) {
      rows_extra = lim[[1]][n.fac(proj)+1]:lim[[2]][n.fac(proj)+1]
      rows = c(rows, rows_extra)
    }

    if (neib.by.group) {
      coor_group = dplyr::left_join(data.frame(row=rows), proj$space, by = "row")
      neib = neib_voronoi(coor_group)
    } else {
      neib = neib_voronoi(proj$space)[rows,rows]
    }

    car = car(neib) * tau
    m = matrix(0, proj$data$dim$row, proj$data$dim$row)
    m[rows,rows] = car
    cov[[fac]] = m + diag(.01, proj$data$dim$row)
  }

  cov
}


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



proj =
  blank_project() |>
  set_info("dados de temperatura max semanal") |>
  set_data(simdata = T, group.sizes = rep(30,3), columns = 4, semi.conf = F) |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F) |>
  {\(.) {.$prior$alpha$cov   = .$prior$alpha$cov |> lapply(function(x) {x[x == 10] = 100; x}) ; . }}() |>
  #{\(.) {.$prior$alpha$cov   = car_alpha_local(., 250) ; . }}() |>
  #{\(.) {.$prior$alpha$cov   = .$prior$alpha$cov  |> lapply(function(x) car_alpha(neib_voronoi(.$space), x, 4)) ; . }}() |>
  {\(.) {.$prior$lambda$cov  = .$prior$lambda$cov |> lapply(function(x) car(neib_simple(ncol(x)))) ; . }}() |>
  set_fit(iter = 1000, seed = 12345)

