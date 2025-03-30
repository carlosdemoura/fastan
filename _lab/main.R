df = readRDS("C:/Users/Carlos/Downloads/projetos/proj_tmax_semiconf_100.rds")
model = df$model
model$data[sample(1:nrow(model$data), size = 100), 1] = NA

model$pred =
  model$data |>
  {\(.) dplyr::filter(., is.na(.$value))}()

model$data =
  model$data |>
  {\(.) dplyr::filter(., !is.na(.$value))}()

proj = list()
proj$model = model
proj$fit = run_stan(proj$model, iter = 100)

saveRDS(proj, "C:/Users/Carlos/Downloads/proj_c_pred.rds")
