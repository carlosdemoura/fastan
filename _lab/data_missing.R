library(dplyr)
library(data.table)
limit_p = .3


discard_rows = c()
discard_cols = c()

model$data = model$data_fa
mod = model

m = mod$data |>
  tidyr::pivot_wider(names_from = col, values_from = value)

for (i in 1:nrow(m)) {

}

for (j in 1:ncol(m)) {

}


load(local_data("projects/fastan_project_1.Rdata"))

for (i in 1:nrow(m)) {
  missings = is.na(m[i,])
  if ( sum(missings) > limit_general * ncol(m) ) {
    discard_lines = append(discard_lines, m[])
  } else if ( (sum(missings) > 0) & (sum(missings) < limit_general * ncol(m)) ) {

    df = data.frame(type = ifelse(is.na(m[i,]), "missing", "present"),
                    n = rep(1, ncol(m)))

    df = df %>%
      mutate(group = rleid(type)) %>%
      group_by(group, type) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      select(-group)

    # Decision
    if ( max(df[df$type == "missing",]$n) >= limit_local ) {  # Se os dados estão muito agrupados
      discard_lines = append(discard_lines, m[])
    } else if ( ) {  # Se há muitos missings nas caudas

    }

  } else {
    # Do nothing
  }

}


m = data.frame(
  c1 = c(1 , 4 , 9 , 16, 25, 36, NA, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400),
  c2 = c(1 , 4 , 9 , 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400),
  c3 = c(1 , 4 , 9 , 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400),
  c4 = c(1 , 4 , 9 , 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400),
  c5 = c(1 , 4 , 9 , 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400),
  c6 = c(1 , 4 , 9 , 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400)
)

m = t(as.matrix(m))
rownames(m) = NULL





hide_data = function(mod, p) {
  n = floor(p * nrow(mod$data))
  s = sample(nrow(mod$data), size = n, replace = F) |>
    sort()

  mod$real$pred = mod$data[s,]

  mod$data[s,]$value = mod$sentinel + 3

  return(mod)
}


fiat_prediction = function(data, cicles = 1) {

}






#' Title
#'
#' @param mod
#' @param p
#'
#' @return
#'
#' @examples
#'
#' @export
hide_data = function(mod, p) {
  n = floor(p * nrow(mod$data))
  s = sample(nrow(mod$data), size = n, replace = F) |>
    sort()

  mod$real$pred = mod$data[s,]

  mod$data[s,]$value = mod$sentinel + 3

  return(mod)
}


