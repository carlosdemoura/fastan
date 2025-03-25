library(tidyverse)
devtools::load_all()


project = list(info = "")
project$model = generate_data_sc(rep(5, 3), 10, 1, T)

set.seed(12345)
fit1 = run_stan(project$model, iter = 200, warmup = 100, chains = 2, seed = 123)
draws1 = my_extract(fit1)

fit2 = run_stan(project$model, iter = 200, warmup = 100, chains = 2, init = fiat_init_from_draws(draws1))
draws2 = my_extract(fit2)

draws12 = merge_draws(draws1, draws2)

set.seed(12345)
fit3 = run_stan(project$model, iter = 400, warmup = 100, chains = 2, seed = 123)
draws3 = my_extract(fit3)



par = "alpha"
i = 198
draws3 [[par]][i:(i+8),,1,1]
draws12[[par]][i:(i+8),,1,1]

draws1[[par]][200,,1,1]
draws2[[par]][1:5,,1,1]


init = fiat_init_from_draws(draws1)
init[[1]]$sigma2[1,1]
init[[2]]$sigma2[1,1]





dim(draws3$sigma2)




draws1 = get_all_chains(fit1)
draws3 = get_all_chains(fit3)



project$summary = summary_matrix(project$fit)

save(project, file = "project.rds")
load("project.rds")












library(tidyverse)
devtools::load_all()


project = list(info = "")
project$model = generate_data_sc(rep(5, 3), 10, 1, T)
set.seed(12345)
fit1 = run_stan(project$model, iter = 300, warmup = 100, chains = 2)
fit2 = run_stan(project$model, iter = 300, warmup = 100, chains = 2, init = )
set.seed(12345)
fit3 = run_stan(project$model, iter = 600, warmup = 100, chains = 2)

draws1 = get_all_chains(fit1)
draws3 = get_all_chains(fit3)

x = my_extract(fit)
z = rstan::extract(fit, permuted = F, inc_warmup = T)


project$summary = summary_matrix(project$fit)

#save(project, file = "project.rds")
#load("project.rds")



