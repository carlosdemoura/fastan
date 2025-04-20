#library(ggplot2)
#ibrary(ggridges)

#' Plot contrast matrix
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_contrast = function(smry, par = "alpha", stat = "mean") {
  df = smry[[par]][,,stat, drop=F] |>
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
#' @export
#'
#' @import ggplot2
plot_hpd = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = matrix_to_df(smry[[par]])
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()
  df = df[df[[loc.name]] == get(loc.name), ]
  real = "real" %in% colnames(df)

  y_min = min(smry[[par]][,,"hpd_min"])
  y_max = max(smry[[par]][,,"hpd_max"])

  ggplot(df) +
    {if (loc.name == "row") { aes(x = col) }
      else { aes(x = row) } } +
    geom_errorbar(aes(ymin = hpd_min, ymax = hpd_max, color = "HPD"), width = .6, linewidth = .6) +
    {if ("median" %in% stat) geom_point(aes(y = median, color = "Median"), size = 2, shape = 17)} +
    {if ("mean"   %in% stat) geom_point(aes(y = mean  , color = "Mean")  , size = 2, shape = 16)} +
    {if ("real"   %in% stat) geom_point(aes(y = real  , color = "Real")  , size = 2, shape = 15)} +
    labs(
      x = ifelse(loc.name == "row", "col", "row"),
      y = "Values",
      title = paste(par, "posterior", loc.name, c(row, col)),
      color = "Legend"
    ) +
    ylim(y_min, y_max) +
    {if (real) { scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue", "Real" = "green")) }
      else { scale_color_manual(values = c("HPD" = "black", "Mean" = "red", "Median" = "blue")) } } +
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
#' @export
#'
#' @import ggplot2
plot_lambda = function(smry, stat = "mean") {
  df = matrix_to_df(smry$lambda)

  ggplot(df, aes(x = col, group = factor(row), color = factor(row), fill = factor(row))) +
    geom_ribbon(aes(ymin = hpd_min, ymax = hpd_max), alpha = 0.2, color = NA) +
    {if ("mean" %in% stat)   geom_line(aes(y = mean)  , size = 1)} +
    {if ("median" %in% stat) geom_line(aes(y = median), size = 1)} +
    {if ("real" %in% stat)   geom_line(aes(y = real)  , size = 1, color = "green")} +
    scale_color_brewer(name = "Factor", palette = "Set1") +
    scale_fill_brewer(name = "Factor", palette = "Set1") +
    labs(
      title = "lambda posterior (HPD & mean)",
      x = "Column",
      y = "Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))
}


#' Plot posterior sample histogram and/or density
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
plot_posterior = function(fit, par, row = 1, col = 1, type = c("hist", "dens")) {
  df = rstan::extract(fit)[[par]] |>
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
    {if ("hist" %in% type) geom_histogram(aes(y = ..density..), fill = "grey")} +
    {if ("dens" %in% type) geom_density(lwd = 1.5, col = "red")} +
    #{if ("real" %in% type) geom_density(lwd = 1.5, col = "red")} +
    {if (par != "lp__") ylim(0, y_max)} +
    xlim(floor(min(df$x)), ceiling(max(df$x))) +
    theme_minimal()
}


#' Posterior sample trace plot
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
#' @import rstan
plot_trace = function(fit, par, row = 1, col = 1){
  par_ = par |>
    {\(.) if (par == "lp__") .
      else paste0(., "[", row, ",", col, "]")}()

  df = get_chains_mcmc(fit, par_) |>
    sapply(function(x) {x |> c()}) |>
    as.data.frame() |>
    rename_with(~ sub("^V", "chain ", .x)) |>
    {\(.) dplyr::mutate(
      .,
      iteration = 1:nrow(.)
    )}() |>
    tidyr::pivot_longer(names_to = "chain", values_to = "value", cols = tidyr::starts_with("chain"))

  ggplot(df, aes(x = iteration, y = value, color = chain)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = paste0("MCMC for ", ifelse(par == "lp__", "Log-likelihood", paste0(par, "[", row, ",", col, "]"))),
      x = "Iteration",
      y = "Value"
    ) +
    theme_minimal()
}


#' Plot Neff or Rhat
#'
#' @inheritParams plot_mock_doc
#'
#' @import dplyr
#' @import ggplot2
plot_diagnostic = function(fit, stat, par = NULL) {
  stopifnot("stat must bet either 'rhat' or 'neff'" = stat %in% c("rhat", "neff"))

  df = diagnostic_statistics(fit) |>
    {\(.) if(!is.null(par)) dplyr::filter(., .$par == !!par)
      else .}() |>
    dplyr::select(dplyr::all_of(stat)) |>
    `colnames<-`("x")

  title =
    "Histogram of" |>
    paste(stat) |>
    {\(.) if(is.null(par)) paste(., "for all parameters")
      else paste(., "for", par) }()

  ggplot(df, aes(x = x)) +
    geom_histogram(fill = "grey", color = "black") +
    labs(
      title = title,
      x = stat,
      y = "Frequency"
    ) +
    theme_minimal()
}


#' FALTA MUITO
#'
#' Plot ridges
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggridges plot
#'
#' @export
#'
#' @import ggplot2
#' @import ggridges
plot_ridges = function(mod) {
  # FALTA AGRUPAR POR LINHA DATA FA
  ggplot(mod$data, aes(x = value, y = factor(row))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    theme_minimal()
}


#' Plot missing values pattern
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
plot_missing = function(mod, grid = T) {
  df =
    mod$pred |>
    dplyr::mutate(
      missing = factor(TRUE, levels = c(TRUE, FALSE)),
      row     = factor(row, levels = rev(unique(row))),
    )

  # Add line with present value
  for (i in 1:length(unique(df$row))) {
    for (j in 1:length(unique(df$col))) {
      r = unique(df$row)[i]
      c = unique(df$col)[j]
      if (dplyr::filter(df, row == r, col == c) |> nrow() == 0) {
        df =
          df |>
          dplyr::bind_rows(tibble::tibble(
            value = NA,
            group = 1,
            col = c,
            row = factor(r),
            missing = factor(FALSE, levels = c(TRUE, FALSE))
          ))
        stop = T
        break
      }
    }
    if(stop){break}
  }

  row_labels = levels(df$row)
  row_labels[seq_along(row_labels) %% 5 < 4] = ""

  ggplot(df, aes(col, row, fill = missing)) +
    geom_tile() +
    scale_fill_manual(
      values = c("FALSE" = "white",   "TRUE" = "black"),
      labels = c("FALSE" = "Present", "TRUE" = "Missing"),
      guide = guide_legend(override.aes = list(color = "black", size = 1))
    ) +
    { if (grid){theme_minimal()} else {theme_classic() + theme(axis.ticks.y=element_blank())} } +
    scale_y_discrete(labels = row_labels) +
    labs(x = "Column", y = "Row", fill = "", title = "Missing pattern", subtitle = "Considering only rows with missings") +
    theme(legend.position = "bottom", legend.direction = "horizontal")
}


#' ESSA PORRA TÁ CERTA?
#'
#' Print LaTeX table
#'
#' @inheritParams plot_mock_doc
#'
#' @export
print_table = function(smry, par, row = NULL, col = NULL, stat = c("mean", "median")) {
  df = matrix_to_df(smry[[par]])
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()
  df = df[df[[loc.name]] == get(loc.name), c("row", "col", stat)]
  df$loc = get(loc.name)
  real = "real" %in% colnames(df)

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


#' Transform 3D matrix into elongated data frame
#'
#' @param m matrix 3d, the 3rd dim will be elongated.
#'
#' @return `data.frame()`
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
matrix_to_df = function(m) {
  m |>
    {\(x)
      do.call(rbind, lapply(dimnames(x)[[3]], function(slice) {
        x[, , slice, drop = FALSE] |>
          as.data.frame() |>
          {\(.) `colnames<-`(., 1:ncol(.)) }() |>
          {\(.) dplyr::mutate(., row = row.names(.)) }() |>
          {\(.) tidyr::pivot_longer(., names_to = "col", values_to = "v", cols = 1:ncol(x)) }() |>
          {\(.) dplyr::mutate(., stat = slice) }()
      }))
    }() |>
    tidyr::pivot_wider(values_from = "v", names_from = "stat") |>
    {\(.) dplyr::mutate(
      .,
      row = as.numeric(.$row),
      col = as.numeric(.$col)
    )}()
}


#' Mock function for the sole purpose of documentation
#'
#' @param fit .
#' @param smry .
#' @param mod .
#' @param par .
#' @param row .
#' @param col .
#' @param type .
#' @param stat .
#' @param warmup .
plot_mock_doc = function(fit, smry, mod, par, row, col, type, stat, warmup) {}
