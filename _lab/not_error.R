# nao retorna erro por causa do simdata prior

proj =
  blank_project() |>
  set_data(simdata = T, seed = 12345, group.sizes = rep(20,3), columns = 10, semi.conf = F, pred = .1) |>
  set_space(type = "random", cont = T) |>
  set_prior(type = "normal", semi.conf = T, engine = list(alpha = function(x) car_conditional(x, neib_voronoi, 400), lambda = car_simple)) |>
  set_data(simdata = "prior", seed = 12345) |>
  set_fit2(iter = 100, seed = 12345)
