lambda_cov = function(col, rho = .95) {
  D =
    rep(2, col-2) |>
    {\(.) c(1, ., 1)}() |>
    diag()

  W = matrix(0, nrow = col, ncol = col)
  W[1,2] = 1
  W[col,col-1] = 1
  for (i in 2:(col-1)) {
    W[i, c(i-1, i+1)] = 1
  }

  solve(D - rho * W)
}


lambda_mean = function(col) {
  rep(0, col)
}


run_stan3 = function(model, init = NULL, chains = 1, ...) {
  if (is.null(init)) {
    init = fiat_init(model, chains)
  }
  data = adjust_data_interface(model)
  data[["lambda_cov"]]  = lambda_cov(data$al_col)
  data[["lambda_mean"]] = lambda_mean(data$al_col)
  rstan::stan(file   = system.file("stan", "interface_fa_dependence3.stan", package = "fastan"),
              data   = data,
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
  )
}


run_stan2 = function(model, init = NULL, chains = 1, ...) {
  if (is.null(init)) {
    init = fiat_init(model, chains)
  }
  rstan::stan(file   = system.file("stan", "interface_fa_dependence.stan", package = "fastan"),
              data   = adjust_data_interface(model),
              pars   = c("alpha", "lambda", "sigma2") |> {\(.) if (!is.null(model$pred)) append(., "pred") else .}(),
              init   = init,
              chains = chains,
              ...
  )
}













devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana)

p_missing_max = .2

stations_missings =
  df %>%
  filter(missing) %>%
  mutate(
    missing = is.na(temp_max) |> as.numeric()
  ) %>%
  group_by(station.id) %>%
  summarise(missing_p = mean(missing),
            .groups = "drop")

ellegilbe_stations =
  stations_missings %>%
  filter(missing_p <= p_missing_max) %>%
  select(station.id) %>%
  c() %>%
  purrr::pluck(1)

df =
  df %>%
  filter(!missing | station.id %in% ellegilbe_stations) %>%
  arrange(alt_tipo)

proj       = list()
proj$info  = "Dados de temperatura max semanal com dados faltantes; modelo semi-confrmatório por tipo de altitude; com predição"
proj$model = fastan::model_data_sc(df, "temp_max", "alt_tipo", "station.id", "semana", T)

proj$model$pred =
  proj$model$data |>
  {\(.) dplyr::filter(., is.na(.$value))}()

proj$model$data =
  proj$model$data |>
  {\(.) dplyr::filter(., !is.na(.$value))}() %>%
  dplyr::filter(row %in% proj$model$pred$row)

proj$fit = run_stan2(proj$model, iter = 5000, warmup = 2000, seed = 12345)
proj$summary = fastan::summary_matrix(proj$fit)

saveRDS(proj, "C:/Users/Carlos/Downloads/pred.rds")





df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") %>%
  filter(!missing) %>%
  arrange(regiao, station.id, semana) %>%
  filter(regiao == "Sul")
tmax_conf       = list()
tmax_conf$info  = "dados de temperatura max semanal; modelo confirmatório; grupos por região"
tmax_conf$model =
  fastan::model_data_sc(
    arrange(df, regiao, station.id, semana),
    value = "temp_max",
    group = "regiao",
    row  = "station.id",
    col  = "semana",
    semi.conf = F
  )
tmax_conf$fit     = run_stan2(tmax_conf$model, iter = 1000, warmup = 200, chains = 1, seed = 12345)
tmax_conf$summary = fastan::summary_matrix(tmax_conf$fit)

saveRDS(tmax_conf, "C:/Users/Carlos/Downloads/sul.rds")
