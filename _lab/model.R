compiled_model <- rstan::stan_model("your_model.stan")
saveRDS(compiled_model, "compiled_model.rds")
compiled_model <- readRDS("compiled_model.rds")


compiled_model = system.file("stan", "compiled_model.rds", package = "fastan")
rstan::sampling(compiled_model,
                data   = data,
                pars   = pars,
                init   = init,
                chains = chains,
                ...
                )
