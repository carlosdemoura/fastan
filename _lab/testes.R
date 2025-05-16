smry = simdata$summary
par = "alpha"
col = 1
row = 1:175 |> c(327:482)
col = 2
row = 176:482
smry[[par]][row,col,"real"] |>
  {\(.) (. >= smry[[par]][row,col,"hpd_min"]) & (. <= smry[[par]][row,col,"hpd_max"])}() |>
  as.numeric() |>
  {\(x) c(mean(x), length(x))}()

fit = tmax_expl$fit


x =
  fit |>
  rstan::extract(permuted = F) |>
  apply(c(2, 3), function(x) {coda::geweke.diag(x) |> purrr::pluck(1) |> unname()} ) |>
  as.vector()
sumary(x)
max(x)





proj = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_tmax_pred.rds")
mod = proj$model

df1 =
  mod$pred %>%
  mutate(
    missing = factor(TRUE, levels = c(TRUE, FALSE)),
  )

df2 =
  mod$data %>%
  dplyr::filter(row %in% df$row) %>%
  mutate(
    missing = factor(FALSE, levels = c(TRUE, FALSE))
  )

df =
  rbind(df1, df2) %>%
  mutate(
    row = factor(row, levels = rev(unique(row)))
  )



row_labels = levels(df$row)
row_labels[seq_along(row_labels) %% 5 < 4] = ""
p=
ggplot(df, aes(col, row, fill = missing)) +
  geom_tile() +
  scale_fill_manual(
    values = c("FALSE" = "white",   "TRUE" = "black"),
    labels = c("FALSE" = "Present", "TRUE" = "Missing"),
    guide = guide_legend(override.aes = list(color = "black", size = 1))
  ) +
  scale_y_discrete(labels = row_labels) +
  theme_minimal() +
  labs(x = "Column", y = "Row", fill = "", title = "Missing pattern")

p%>%ggplotly()
