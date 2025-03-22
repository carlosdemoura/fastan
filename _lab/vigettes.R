
fl = 30  # 1 chain
fl = 33  # 2 chains
project = "D:/carlos/01_pesquisa/2024_bayes/pacote/analiseFatorial/simulations/simulationResults_" |> paste0(fl)
load(file.path(project, "output.Rdata"))
output = stan.output
View(output)

?rstan::get_elapsed_time(output)





coda::gelman.plot(get_chains_mcmc(project, "lp__"))
coda::gelman.diag(get_chains_mcmc(project, "lp__"))
coda::geweke.diag(get_chains_mcmc(project, "lp__"))


rstan::traceplot(output, pars = "lp__")



plot_density(stan.samp, data, "pred", row.arg = i, col.arg = j)
plot_density(stan.samp, data, "alpha", row.arg = 4, col.arg = 1)


n = 3
par(mfrow = c(n, n))
linhas_sorteadas = sample(nrow(data$x), n)
colunas_sorteadas = sample(ncol(data$x), n)
for (i in linhas_sorteadas) {
  for (j in colunas_sorteadas) {
    plot_density(stan.samp, data, "pred", row.arg = i, col.arg = j)
    #abline(v = new_obs[i,j], lwd = 3)
  }
}


plot_density(stan.samp, "pred", row.arg = 4, col.arg = 1)
