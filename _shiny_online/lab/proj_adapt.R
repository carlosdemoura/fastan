from = "D:/carlos/01_pesquisa/2024_bayes/2503_marco/"
to   = "D:/carlos/01_pesquisa/fastan/_shiny_online/"
to   = "C:/Users/Carlos/Downloads/projetos/"
proj = readRDS(paste0(from, "conf_tmax_proj.rds"))


proj$fit = proj$fits[[1]]
proj$fits = NULL
proj$summary = summary_matrix(proj$fit)

saveRDS(proj, paste0(to, "proj_tmax_semiconf_100.rds"))
proj$fits[[1]]@stan_args







from   = "C:/Users/Carlos/Downloads/projetos/"
to   = "C:/Users/Carlos/Downloads/projetos2/"

fl = "proj_tmax_conf_p1.rds"
fl = "proj_tmax_conf_p2.rds"
fl = "proj_tmax_semiconf.rds"
fl = "proj_tmax_semiconf_100.rds"

proj = readRDS(paste0(from, fl))
proj$draws = my_extract(proj$fit) |> lapply(function(x) { x[(dim(x)[1]*.5):dim(x)[1],,,,drop=F] })
proj$fit = NULL
saveRDS(proj, paste0(to, fl))



proj = readRDS(paste0(to, fl))
