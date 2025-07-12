#pkgs =
c(
  "abind",
  "coda",
  "clue",
  "deldir",
  "dplyr",
  "geosphere",
  "gridExtra",
  "plotly",
  "posterior",
  "purrr",
  "rstan",
  "stats",
  "tibble",
  "tidyr",
  "utils",
  "zip",
  "testthat",
  "distributional",
  "ggplot2",
  "shiny"
  ) |>
  setdiff(installed.packages()) |>
  install.packages()

# for (pkg in pkgs) {
#   if (!(pkg %in% installed.packages())) {
#     install.packages(pkg)
#   }
# }

