#' Plot basic map structure
#'
#' @param proj `fastan::project`
#'
#' @return `ggplot2`/`gridExtra` object.
#'
#' @export
#'
#' @import ggplot2
plot_map = function(proj) {
  df = proj$space

  margin = 1/2
  xrange = range(df$lon, na.rm = TRUE) + c(-margin, margin)
  yrange = range(df$lat, na.rm = TRUE) + c(-margin, margin)

  ggplot() +
    geom_polygon(data = map_data("world"),
                 aes(x = .data$long, y = .data$lat, group = .data$group),
                 fill = "lightblue", color = "black", lwd = .2) +
    #geom_point(data = df, aes(x = .data$lon, y = .data$lat), size = 1.5) +
    coord_fixed(xlim = xrange, ylim = yrange) +
    theme(
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks.y     = element_blank(),
      axis.title.y     = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_blank()
    )
}


#' Plot map posterior statistics
#'
#' @inheritParams plot_mock_doc
#'
#' @return `ggplot2`/`gridExtra` object.
#'
#' @export
#'
#' @import ggplot2
plot_map_post = function(proj, par, col = 1, stat) {
  df = proj$space
  df$x = proj$summary[[par]][,col,stat]
  plot_map(proj) +
    geom_point(data = df, aes(x = .data$lon, y = .data$lat, color = .data$obs), size = 1.5) +
    scale_color_viridis_c(option = "turbo") +
    labs(title = paste0(par, "[,",col,"]", " posterior ", stat))
}


#' Plot map posterior association
#'
#' Useful for semi-confimatory models.
#'
#' @param proj `fastan::project`.
#' @param extra.only logical, `TRUE` if only the entries from the extra group should be plotted, `FALSE` otherwise.
#' @param r numeric, default = 1, radius of the dots.
#'
#' @return `ggplot2`/`gridExtra` object.
#'
#' @export
#'
#' @importFrom dplyr filter group_by_at mutate ungroup
#' @import ggplot2
#' @importFrom ggforce geom_arc_bar
#' @importFrom tidyr pivot_longer
#' @importFrom utils tail
plot_map_post_factor = function(proj, extra.only = F, r = 1) {
  rows.extra = utils::tail(fiat_groups_limits(proj$data$dim$group.sizes)[[1]], 1):utils::tail(fiat_groups_limits(proj$data$dim$group.sizes)[[2]], 1)
  df =
    (1 - proj$summary$alpha[,,"hpd_contains_0"]) |>
    as.data.frame() |>
    {\(.) `colnames<-`(., paste0("factor ", 1:ncol(.)))}() |>
    {\(.) cbind(proj$space, .)}() |>
    {\(.) if (extra.only) .[rows.extra,] else .}() |>
    tidyr::pivot_longer(cols = starts_with("factor"), names_to = "factor", values_to = "value") |>
    dplyr::filter(.data$value == 1) |>
    dplyr::group_by_at("id") |>
    dplyr::mutate(
      n = n(),
      angle_start = (row_number() - 1) * 2 * pi / n,
      angle_end   = row_number() * 2 * pi / n
    ) |>
    dplyr::ungroup()

  #colors = scales::hue_pal()(length(unique(df$factor)))
  #names(colors) = unique(df$factor)

  p = plot_map(proj)
  p$layers[[2]] = NULL

  p +
    ggforce::geom_arc_bar(
      data = df,
      aes(
        x0 = .data$lon, y0 = .data$lat, r0 = 0, r = r,
        start = .data$angle_start, end = .data$angle_end,
        fill = .data$factor
      ),
      color = NA
    ) +
    #scale_fill_manual(values = colors) +
    labs(fill = "Legend", x = "x", y = "y")
}


#' Plot map data
#'
#' @inheritParams plot_mock_doc
#'
#' @return `ggplot2`/`gridExtra` object.
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @importFrom stats var
plot_map_data = function(proj, stat) {
  stopifnot("stat must be group, mean or var" = stat %in% c("group", "mean", "var"))
  df =
    proj$data$obs |>
    dplyr::group_by_at("row") |>
    dplyr::summarise(
      mean = mean(.data$value),
      var = stats::var(.data$value)
    ) |>
    dplyr::mutate(
      group = unique(proj$data$obs[,c("row", "group")]) |> {\(.) .$group}() |> factor()
    ) |>
    dplyr::left_join(proj$space, by = "row")


  plot_map(proj) +
    geom_point(data = df, aes(x = .data$lon, y = .data$lat, color = .data[[stat]]), size = 1.5) +
    {
      if (stat == "group") scale_color_viridis_d(option = "B") else scale_color_viridis_c(option = "turbo")
    } +
    labs(title = paste0("data rowise ", stat))
}
