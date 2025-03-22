#project = fa_project(data, "")

set.seed(12345)

project = list(info = "")

project$model = generate_data_sc(rep(10, 3), 20, 1, T)

#project = list(model = model_data_sc)

project$fit = run_stan(project$model, iter = 2000, warmup = 1000, chains = 2)

project$summary = summary_matrix(project$fit)

project$summary = summary_matrix(project$fit, project$model)

save(project, file = "project.rds")
load("project.rds")

#smry = project$summary; par = "alpha"; row = 1; col = 1; type = c("hist", "dens"); fit = project$fit
