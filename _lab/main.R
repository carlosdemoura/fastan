library(BayesianTools)
library(posterior)

library(tidyverse)
library(coda)
library(ggplot2)
library(lattice)
library(rstan)


df = data.frame(row = data$pred$rows,
                col = data$pred$cols,
                real = numeric(length(data$pred$cols)))
for (i in 1:nrow(df)) {
  df[i,]$real = data$pred$x[df$row[i], df$col[i]]
}

data$pred$df = df


set.seed(12345)
data = fiat_data(groups = 1, cicles = 1, rows_by_group = 10, columns = 8, semi.conf = F)
#data = fiat_data(groups = 2, rows_by_group = 50, columns = 100, semi.conf = F)
data = hide_data(data, .1)


set.seed(12345)
run_simulation(data = data,
               path_interface = "D:/carlos/01_pesquisa/2024_bayes/pacote/analiseFatorial",
               path_dump = "D:/carlos/01_pesquisa/2024_bayes/pacote/analiseFatorial/simulations",
               iter = 1000,
               warmup = 500,
               chains = 2, #########################
               save.data = T
)




path_root = "D:/2024_bayes"
path_root = "D:/carlos/01_pesquisa/2024_bayes"

estacoes = read.csv( file.path(path_root, "bancos/dados_tratados", "estacoes.csv") )

fl = 19
load(file = paste0(path_root, "/pacote/analiseFatorial/simulations/simulationResults_", fl, "/samp.Rdata"))
load(file = paste0(path_root, "/pacote/analiseFatorial/simulations/simulationResults_", fl, "/output.Rdata"))
load(file = paste0(path_root, "/pacote/analiseFatorial/simulations/simulationResults_", fl, "/data.Rdata"))
samp = stan.samp
output = stan.output

inf_m = fiat_inference_matrix(stan.samp, data)
inf_df = fiat_inference_df(stan.samp, data)

plot_hpd(inf_df, "alpha", 1)

plot_hpd(inf_df, "lambda", 1)

plot_hpd(inf_df, "sigma2", 1, stats = c("mean"))

plot_hpd(inf_df, "pred")

#ggsave("sigma2.png", width = 15, height = 5, dpi = 300, bg = "white")

heatmap(data$x == data$sentinel[1])
