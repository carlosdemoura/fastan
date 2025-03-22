#library(ggplot2)
#ibrary(ggridges)

#' Plot contrast matrix
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
plot_contrast = function(smry, par = "alpha", stat = "mean") {

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
}


#' Plot HPD interval & other stats
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
plot_hpd = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = as_data_frame(smry, par, row, col, c(stat, "hpd_min", "hpd_max"))
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
}


#' Plot lambdas
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
plot_lambda = function(smry) {
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
}


#' Plot posterior sample histogram and/or density
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
plot_posterior = function(output, par, row, col, type = c("hist", "dens")) {
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
}


#' Posterior sample trace plot
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
plot_trace = function(output, par, row, col, warmup = T){
  par = par |>
    {\(.) if (par == "lp__") .
      else paste0(., "[", row, ",", col, "]")}()

  rstan::traceplot(output, pars = par, inc_warmup = warmup)
}


#' Title
#'
#' @param mod .
#'
#' @return
#'
#' @examples
#'
#' @export
#'
#' @import ggplot2
#' @import ggridges
plot_ridges = function(mod) {
  # FALTA AGRUPAR POR LINHA DATA FA
  ggplot(mod$data_fa, aes(x = value, y = factor(row))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    theme_minimal()
}


#' Print LaTeX table
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import ggplot2
print_table = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = as_data_frame(smry, par, row, col, stat)
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()

  cat(
    "\\begin{center}",
    paste("Summary of the MCMC for the posterior distribution of", par, ifelse(loc.name == "col", "column", "row"), c(row, col), "\\\\"),
    paste0(c("\\begin{tabular}{|", rep("m{1.5cm}|", length(stat)), "}"), collapse = ""),
    "\\hline",
    sep = "\n")

  cat("\\textbf{", ifelse(loc.name != "col", "Column", "Row") ,"}", sep = "")
  for (each in stat) {
           if (each == "real"   ){cat(" & \\textbf{Real value}"    , sep = "")
    } else if (each == "mean"   ){cat(" & \\textbf{Mean}"          , sep = "")
    } else if (each == "median" ){cat(" & \\textbf{Median}"        , sep = "")
    } else if (each == "sd"     ){cat(" & \\textbf{St dev}"        , sep = "")
    } else if (each == "hpd_min"){cat(" & \\textbf{HPD LB}"        , sep = "")
    } else if (each == "hpd_max"){cat(" & \\textbf{HPD UP}"        , sep = "")
    } else if (each == "hpd_amp"){cat(" & \\textbf{HPD amplitude}" , sep = "")}
  }
  cat(
    "\\\\",
    "\\hline",
    sep = "\n")

  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){

      cat(round(as.numeric(df[i,j]), 2))
      if (j < ncol(df)){
        cat(" & ")
      }

    }
    cat("\\\\\n\\hline\n")
  }

  cat(
    "\\end{tabular}",
    "\\end{center}",
    sep = "\n")
}


#' Title
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @examples 5*2
#'
#' @export
#'
#' @import dplyr
as_data_frame = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  smry[[par]] |>
    {\(.) if (!is.null(col))  .[,col,]
      else if (!is.null(row)) .[,row,]}() |>
    as.data.frame() |>
    {\(.) dplyr::mutate(., loc = row.names(.))}() |>
    dplyr::select(dplyr::all_of(c("loc", stat)))
}


#' Mock function for the sole purpose of documentation
#'
#' @param output .
#' @param par .
#' @param row .
#' @param col .
#' @param type .
#' @param stat .
#' @param smry .
#' @param warmup .
plot_mock_doc = function() {}
