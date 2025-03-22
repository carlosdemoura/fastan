#' Title
#'
#' @param f
#'
#' @return
#'
#' @export
plot_decorator = function(f) {
  new = function(..., save = F) {
    p = f(...)
    if (save) {

    }
    print(p)
  }
  return(new)
}


#' Plot contrast matrix
#'
#' @export
plot_contrast = plot_decorator(function(smry, par = "alpha", stat = "mean") {

  df = smry[[par]][,,stat, drop=F] |>
    as.matrix() |>
    as.data.frame() |>
    {\(.) `colnames<-`(., 1:ncol(.))}() |>
    {\(.) dplyr::mutate(., row = as.numeric(rownames(.))) }() |>
    tidyr::pivot_longer(cols = -row, names_to = "factor", values_to = "value")

  if (stat == "hpd_contains_0") {
    df$value = ifelse(df$value, "No", "Yes")
  }

  ggplot(df, aes(factor, row, fill = value)) +
    geom_tile() +
    {if (stat == "hpd_contains_0") {
      scale_fill_manual(values = c("Yes" = "black", "No" = "white"),
                        guide = guide_legend(override.aes = list(color = "black", size = 5, stroke = 1)) )
      } else {
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
      }
    } +
    scale_y_reverse() +
    labs(fill = ifelse(stat == "hpd_contains_0", "Factor is significative", stat),
         x = "Factor",
         y = "Row",
         title = "Alpha posterior contrast")
})


#' Plot HPD interval & other stats
#'
#' @export
plot_hpd = plot_decorator(function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  if (!is.null(col)) {
    df = smry[[par]][,col,]
  } else if (!is.null(row)) {
    df = smry[[par]][,row,]
  }

  df = df |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., loc = row.names(.))}()

  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()

  real = "real" %in% colnames(df)

  ggplot(df, aes(x = loc)) +
      geom_errorbar(aes(ymin = hpd_min, ymax = hpd_max, color = "HPD"), width = .6, linewidth = .6) +
      {if ("median" %in% stat) geom_point(aes(y = median, color = "Median"), size = 2, shape = 17)} +
      {if ("mean"   %in% stat) geom_point(aes(y = mean, color = "Mean"), size = 2, shape = 16)} +
      {if (real)                geom_point(aes(y = real, color = "Real"), size = 2, shape = 15)} +
      labs(
        x = ifelse(loc.name == "row", "col", "row"),
        y = "Values",
        title = paste(par, "posterior", loc.name, c(row, col)),
        color = "Legend"
      ) +
      {if (real) { scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue", "Real" = "green")) }
        else{ scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue")) } } +
      theme_minimal() +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
})


#' Plot lambdas
#'
#' @export
plot_lambda = plot_decorator(function(smry) {
  ggplot(matrix_to_df(smry$lambda), aes(x = col, group = factor(row), color = factor(row), fill = factor(row))) +
    geom_ribbon(aes(ymin = hpd_min, ymax = hpd_max), alpha = 0.2, color = NA) +
    geom_line(aes(y = mean), size = 1) +
    scale_color_brewer(name = "Factor", palette = "Set1") +
    scale_fill_brewer(name = "Factor", palette = "Set1") +
    #factors = data$groups$labels
    #scale_color_brewer(name = "Factor", palette = "Set1", labels = factors) +
    #scale_fill_brewer(name = "Factor", palette = "Set1", labels = factors) +
    labs(
      title = "lambda posterior (HPD & mean)",
      x = "Column",
      y = "Value"
    ) +
    theme_minimal()
})


#' Title
#'
#' @export
plot_sample = plot_decorator(function(output, par, row, col, type = c("hist", "dens")) {
  df = rstan::extract(output)[[par]] |>
    {\(.) if (par == "lp__") .
      else .[,row,col]}() |>
    as.data.frame() |>
    `colnames<-`("x")

  y_max = list(
    {ggplot(df, aes(x = x)) + geom_histogram(aes(y = ..density..), binwidth = 1)},
    {ggplot(df, aes(x = x)) + geom_density()}
    ) |>
    sapply( function(x){ x |> {\(.) ggplot_build(.)$data}() |> {\(.) max(.[[1]]$ymax)}() } ) |>
    max()

  ggplot(df, aes(x = x)) +
    {if ("hist" %in% type) geom_histogram(aes(y = ..density..), binwidth = 1, fill = "grey")} +
    {if ("dens" %in% type) geom_density(lwd = 1.5, col = "red")} +
    #{if ("real" %in% type) geom_density(lwd = 1.5, col = "red")} +
    ylim(0, y_max) +
    xlim(floor(min(df$x)), ceiling(max(df$x))) +
    theme_minimal()
})


#' Title
#'
#' @export
plot_trace = plot_decorator(function(output, par, row, col){
  par = par |>
    {\(.) if (par == "lp__") .
      else paste0(., "[", row, ",", col, "]")}()

  rstan::traceplot(output, pars = par, inc_warmup = T)
})


print_table = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  cat("
  \\begin{center}
  Tabela para", post,"
  \\\\
  \\begin{tabular}{|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{2cm}|m{1.5cm}|}
  \\hline
  \\ textbf{Índice} & \\textbf{Valor Real} & \\textbf{Média} & \\textbf{Mediana} & \\textbf{Desvio padrão} & \\textbf{LI IHPD} & \\textbf{LS IHPD} & \\textbf{Amplitude IHPD} \\\\
  \\hline
  ")
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      cat(round(as.numeric(df[i,j]), 4))
      if (j<ncol(df)){
        cat(" & ")
      }
    }
    cat(" \\\\\n\\hline\n")
  }
  cat("
  \\end{tabular}
  \\end{center}
  ")
  print("=================================================================================")
}


#' Title
#'
#' @param ...
plot_mock_doc = function(...) {}
