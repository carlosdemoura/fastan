table = table(datalar$region) |> as.data.frame()

n.groups = nrow(table)
m = c()

for (i in 1:n.groups) {
  lines =
    rep(0.01, times = i-1) |>
    c(10) |>
    c(rep(0.01, times = 5-i)) |>
    rep(times = table$Freq[i])

  m = append(m, lines)
}

m = matrix(m, ncol = n.groups, byrow = T)


rep()
c(10, 1, 1, 1, 1)

rep(, times = x$Freq[1])
x$Freq



x = readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2503_marco/conf_medias_project.rds")
x$fit = readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2503_marco/conf_medias_fit1.rds")[[1]]
x$draws = my_extract(x$fit)
x$summary = summary_matrix(x$fit)

saveRDS(x, file = "C:/Users/Carlos/Downloads/teste1.rds")

y = data.frame(station.id = x$model$labels$loading)

devtools::load_all("D:/carlos/01_pesquisa/meteobr")
merge(y, stations[c("station.id", "region")], on = "station.id")



library(ggplot2)
library(reshape2)

# Exemplo de matriz
mat <- matrix(sample(c(1, 10), 25, replace = TRUE), nrow = 5)

# Converter para data frame longo
df <- melt(var_alpha_prior)
df <- melt(m)

# Plotar heatmap
ggplot(df, aes(Var2, Var1, fill = factor(value))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("1" = "lightblue", "10" = "darkblue")) +
  theme_minimal() +
  labs(x = "Coluna", y = "Linha", fill = "Valor") +
  scale_y_reverse()


df = project$model$data

df = unique(simdata$data[c("row", "group")])

ggplot(df, aes(region, sation.id, fill = factor(region))) +
#ggplot(df, aes(group, row, fill = factor(group))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("1" = "lightblue", "10" = "darkblue")) +
  theme_minimal() +
  labs(x = "Coluna", y = "Linha", fill = "Valor") +
  scale_y_reverse()

df =
  unique(data[c("station.id", "region")]) |>
  `colnames<-`(c("row", "group"))
labels = list(
  row = levels(df$row),
  group = levels(df$group)
)

df |>
  mutate(
    row    = match(row,   labels$row),
    group  = match(group, labels$group)
  )

simdata = generate_data_sc(rep(10,3), 10, 1, T)

df = unique(project$model$data[c("row", "group")])





data = data |> mutate(across(c(station.id, region), as.character), across(week, as.numeric))
