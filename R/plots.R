#' Plot contrast matrix
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr pivot_longer
plot_contrast = function(smry, par = "alpha", stat = "mean") {
  smry = validate_proj_arg(smry, "summary")
  df = smry[[par]][,,stat, drop=F] |>
    as.data.frame() |>
    {\(.) `colnames<-`(., 1:ncol(.))}() |>
    {\(.) dplyr::mutate(., row = as.numeric(rownames(.))) }() |>
    tidyr::pivot_longer(cols = -row, names_to = "factor", values_to = "value")

  if (stat == "hpd_contains_0") {
    df$value = ifelse(df$value, "No", "Yes")
  }

  ggplot(df, aes(.data$factor, .data$row, fill = .data$value)) +
    geom_tile() +
    {if (stat == "hpd_contains_0") {
      scale_fill_manual(values = c("Yes" = "black", "No" = "white"),
                        guide = guide_legend(override.aes = list(color = "black", size = 5, stroke = 1)) )
      } else {
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
      }
    } +
    scale_y_reverse() +
    labs(fill = ifelse(stat == "hpd_contains_0", "Factor is\nsignificative", stat),
         x = "Factor",
         y = "Row",
         title = paste(par, ifelse(stat == "real", "real contrast", "posterior contrast"))) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
      )
}


#' Plot HPD interval & other stats
#'
#' @inheritParams plot_mock_doc
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @importFrom dplyr all_of select
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_hpd = function(smry, par, row = NULL, col = NULL, stat = "mean") {
  stat_plt = function(x) {
    if (x == "shape") {
      list(real = 15, mean = 16, median = 17, mode = 18)
    } else if (x == "color") {
      c("hpd" = "black", "real" = "green", "mean" = "red", "median" = "blue", "mode" = "orange")
    }
  }

  stopifnot("stat must be mean, mode, median or real" = all(stat %in% c("mean", "mode", "median", "real")))

  smry = validate_proj_arg(smry, "summary")
  loc.name = list(row=row,col=col) |> {\(.) names(.)[!sapply(., is.null)]}()
  df =
    summary_as_df(list(summary = smry), par)[[par]] |>
    {\(.) .[.[[loc.name]] == get(loc.name), ]}() |>
    dplyr::select(dplyr::all_of(c("row", "col", "hpd_min", "hpd_max", stat))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(stat), names_to = "stat", values_to = "value")

  # y_min = min(smry[[par]][,,"hpd_min"])
  # y_max = max(smry[[par]][,,"hpd_max"])

  ggplot(df, aes(x = if (loc.name == "row") col else row)) +
    # ylim(y_min, y_max) +
    geom_errorbar(aes(ymin = .data$hpd_min, ymax = .data$hpd_max, color = "hpd"), width = .6, linewidth = .6) +
    geom_point(aes(y = .data$value, color = .data$stat, shape = .data$stat), size = 2) +
    scale_color_manual(values = stat_plt("color")) +
    scale_shape_manual(values = stat_plt("shape")) +
    guides(color = guide_legend(override.aes = list(shape = c(list(hpd = 1), stat_plt("shape")[stat]))),
           shape = "none") +
    labs(
      x = ifelse(loc.name == "row", "col", "row"),
      y = "Values",
      title = paste(par, "posterior", loc.name, c(row, col)),
      color = "Legend"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}


#' Plot lambdas
#'
#' @inheritParams plot_mock_doc
#'
#' @export
#'
#' @importFrom dplyr all_of select
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_lambda = function(smry, stat = "mean") {
  stopifnot(
    "stat must be mean, mode, median or real" = all(stat %in% c("mean", "mode", "median", "real")),
    "only one stat possible" = (length(stat) == 1)
  )
  smry = validate_proj_arg(smry, "summary")
  df =
    summary_as_df(list(summary = smry), "lambda")[["lambda"]] |>
    dplyr::select(dplyr::all_of(c("row", "col", "hpd_min", "hpd_max", stat))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(stat), names_to = "stat", values_to = "value")

  ggplot(df, aes(x = col)) +
    geom_ribbon(
      aes(ymin = .data$hpd_min, ymax = .data$hpd_max, fill = factor(.data$row)),
      alpha = 0.2,
      color = NA,
      show.legend = TRUE
    ) +
    geom_line(
      aes(y = .data$value, color = factor(.data$row), group = interaction(.data$stat, .data$row)),
      linewidth = 1,
      show.legend = FALSE
    ) +
    scale_color_brewer(name = "Factor", palette = "Set1") +
    scale_fill_brewer(name = "Factor", palette = "Set1") +
    labs(
      title = paste0("lambda posterior (HPD & ", paste0(stat, collapse = "& "), ")"),
      x = "Column",
      y = "Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))
}


#' Plot posterior sample histogram and/or density
#'
#' @inheritParams plot_mock_doc
#' @param type .
#'
#' @return ggplot2 plot
#'
#' @export
#'
#' @import ggplot2
#' @importFrom rstan extract
plot_posterior = function(fit, par, row = 1, col = 1, type = c("hist", "dens")) {
  fit = validate_proj_arg(fit, "fit")
  df = rstan::extract(fit)[[par]] |>
    {\(.) if (par == "lp__") .
      else .[,row,col]}() |>
    as.data.frame() |>
    `colnames<-`("x")

  y_max = list(
    {ggplot(df, aes(x = .data$x)) + geom_histogram(aes(y = after_stat(.data$density)), binwidth = 1)},
    {ggplot(df, aes(x = .data$x)) + geom_density()}
    ) |>
    sapply( function(x){ x |> {\(.) ggplot_build(.)$data}() |> {\(.) max(.[[1]]$ymax)}() } ) |>
    max()

  ggplot(df, aes(x = .data$x)) +
    {if ("hist" %in% type) geom_histogram(aes(y = after_stat(.data$density)), fill = "grey")} +
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
#' @import dplyr
#' @importFrom tidyr pivot_longer starts_with
plot_trace = function(fit, par, row = 1, col = 1) {
  fit = validate_proj_arg(fit, "fit")
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

  ggplot(df, aes(x = .data$iteration, y = .data$value, color = .data$chain)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = paste0("MCMC for ", ifelse(par == "lp__", "Log-likelihood", paste0(par, "[", row, ",", col, "]"))),
      x = "Iteration",
      y = "Value"
    ) +
    theme_minimal()
}


#' Plot diagnostic stats
#'
#' @inheritParams plot_mock_doc
#' @param list .
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
plot_diagnostic = function(proj, stat, list = F) {
  stopifnot("stat must bet either 'Rhat', 'n_eff', or 'geweke'" = stat %in% c("Rhat", "n_eff", "geweke"))

  plot_diagnostic_local = function(df, par) {
    title =
      paste(stat) |>
      {\(.) if(par == "all") paste(., "for all parameters")
        else paste(., "for", par) }()

    ggplot(df, aes(x = .data$x)) +
      geom_histogram(fill = "grey", color = "black") +
      labs(
        title = title,
        x = stat,
        y = "#parameters"
      ) +
      theme_minimal()
  }

  if (inherits(proj, "project")) {
    if (!is.null(proj$diagnostic)) {
      df = proj$diagnostic
    } else {
      df = diagnostic(proj)
    }
  } else if (inherits(proj, "stanfit")) {
    df = diagnostic(proj)
  }


  df =
    df |>
    {\(.) dplyr::mutate(., geweke = apply(select(., starts_with("geweke")), 1, function(x) x[which.max(abs(x))])) }() |>
    dplyr::select(dplyr::all_of(c("par", "row", "col", "n_eff", "Rhat", "geweke")))

  plots = list()

  for ( par_ in c("all", setdiff(unique(df$par), "lp__")) ) {
    df_ =
      df |>
      {\(.) if (par_ == "all") .
        else dplyr::filter(., df$par == par_)}() |>
      dplyr::rename(x = stat)
    plots[[par_]] = plot_diagnostic_local(df_, par_)
  }

  plots[["lp__"]] =
    ggplot() +
    annotate("text", x=0, y=0,
             label = df |> dplyr::filter(df$par == "lp__") |> dplyr::select(dplyr::all_of(stat)) |> round(5) |> {\(.) paste(stat, "of lp__\n", .)}()
             )+
    theme_void()

  if (list) {
    return(plots)
  } else {
    gridExtra::grid.arrange(grobs = plots, ncol=2)
  }

}


#' Title
#'
#' @inheritParams plot_mock_doc
#' @param correct .
#'
#' @export
#'
#' @import ggplot2
plot_bias = function(smry, par = "all", correct = T) {
  smry = validate_proj_arg(smry, "summary")

  get_bias = function(par_) {
    x = smry[[par_]]
    if ((par_ == "alpha") & correct) {
      c(x[,,"bias"])[as.logical(x[,,"in_group"])]
    } else {
      c(x[,,"bias"])
    }
  }

  if (par == "all") {
    x = c()
    for (p in names(smry)) {
      x = c(x, get_bias(p))
    }
  } else {
    x = get_bias(par)
  }

  df = data.frame(bias = x)

  ggplot(df, aes(x=.data$bias)) +
    geom_histogram(fill = "grey", color = "black") +
    labs(
      title = paste0("Relative bias for ", ifelse(par == "all", "all parameters", par)),
      x = "relative bias",
      y = "#parameters"
    ) +
    theme_minimal()
}


#' Plot missing values pattern
#'
#' @inheritParams plot_mock_doc
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import tibble
plot_missing = function(data) {
  data = validate_proj_arg(data, "data")
  df =
    data$pred |>
    dplyr::mutate(
      missing = factor(TRUE, levels = c(TRUE, FALSE)),
      row     = factor(row, levels = rev(unique(row))),
    )

  # Add line with present value
  # for (i in 1:length(unique(df$row))) {
  #   for (j in 1:length(unique(df$col))) {
  #     stop = F
  #     r = unique(df$row)[i]
  #     c = unique(df$col)[j]
  #     if (dplyr::filter(df, row == r, col == c) |> nrow() == 0) {
  #       df =
  #         df |>
  #         dplyr::bind_rows(tibble::tibble(
  #           value = NA,
  #           group = 1,
  #           col = c,
  #           row = factor(r),
  #           missing = factor(FALSE, levels = c(TRUE, FALSE))
  #         ))
  #       stop = T
  #       break
  #     }
  #   }
  #   if(stop){break}
  # }

  row_labels = levels(df$row)
  row_labels[seq_along(row_labels) %% 5 < 4] = ""

  ggplot(df, aes(.data$col, .data$row, fill = .data$missing)) +
    geom_tile() +
    scale_fill_manual(
      values = c("TRUE" = "black", "FALSE" = "gray80"),
      labels = c("Missing", "Present"),
      drop = FALSE
    ) +
    scale_y_discrete(labels = row_labels) +
    labs(x = "Column", y = "Row", fill = "", title = "Missing pattern", subtitle = "Considering only rows with missings") +
    theme_classic() +
    theme(legend.position = "bottom", legend.direction = "horizontal", axis.ticks.y = element_blank()) +
    geom_point(aes(text = paste("row", row, "  col", col)), alpha = 0)  # invisible, just for shiny clicks
}


#' Title
#'
#' @inheritParams plot_mock_doc
#' @param loc .
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom grDevices colorRampPalette
plot_normal_prior = function(proj, par, stat, loc = "all") {
  stopifnot("stat must be either mean or cov" = stat %in% c("mean", "cov"))
  if (loc == "all") {
    mat =
      {\(.)
        if (stat == "mean") proj$prior[[par]]$mean
        else {proj$prior[[par]]$cov |> lapply(diag) }
      }() |>
      {\(.) do.call(cbind, .)}()
  } else {
    mat = proj$prior[[par]][[stat]][[loc]] |> as.matrix()
  }

  df =
    as.data.frame(mat) |>
    dplyr::mutate(
      row = 1:nrow(mat)
    ) |>
    tidyr::pivot_longer(-dplyr::all_of("row"), names_to = "col", values_to = "value") |>
    dplyr::mutate(
      row = as.integer(row),
      col = as.integer(gsub("V", "", col))
    )

  vals = sort(unique(df$value))
  if (length(vals) <= 5) {
    palette = grDevices::colorRampPalette(c("white", "black"))(length(vals))
    df$value = factor(df$value, levels = vals)
  }

  ## ticks
  if (par == "alpha") {
    breaks.y =
      proj$data$dim$group.sizes |>
      {\(.) fiat_groups_limits(.)[[1]] |> c(sum(.))}()
  } else {
    breaks.y = c(1, proj$data$dim$col)
  }
  if (stat == "cov") {
    if (loc == "all") {
      breaks.x = 1:ncol(mat)
    } else {
      breaks.x = breaks.y
    }
  } else {
    breaks.x = 1:ncol(mat)
  }

  ## title
  if (loc == "all") {
    x = ifelse(stat == "cov", "var ", "mean ")
    title = bquote(.(x) * .(as.name(par)))
  } else {
    title = bquote(.(paste0(stat, " ")) * .(as.name(par))[.(loc)])
  }

  ggplot(df, aes(x = .data$col, y = .data$row, fill = .data$value)) +
    {
      if ((stat == "mean") & (length(vals) ==1)) geom_tile(color = "black")
      else geom_tile(color = "white")
    } +
    theme_void() +
    theme(
      axis.text.y  = element_text(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.text.x  = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      plot.title   = element_text(hjust = 0.5)
    ) +
    labs(title = title) +
    scale_y_continuous(breaks = breaks.y, trans = "reverse") +
    scale_x_continuous(breaks = breaks.x) +
    {
      #if (length(vals) <= 5) scale_fill_manual(values = setNames(palette, vals), name = "Value")
      if (length(vals) <= 5) scale_fill_manual(values = stats::setNames(palette, vals), name = "Value")
      else scale_fill_gradient(low = "white", high = "black")
    }
}


#' Mock function for the sole purpose of documentation
#'
#' @param proj .
#' @param fit .
#' @param smry .
#' @param data .
#' @param par .
#' @param row .
#' @param col .
#' @param stat .
plot_mock_doc = function(proj, fit, smry, data, par, row, col, stat) {}
