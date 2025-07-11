#' Title
#'
#' @param proj .
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
                 fill = "lightblue", color = "black") +
    geom_point(data = df, aes(x = .data$lon, y = .data$lat), size = 1.5) +
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

#' Title
#'
#' @param proj .
#' @param par .
#' @param col .
#' @param stat .
#'
#' @export
#'
#' @import ggplot2
plot_map_post = function(proj, par, col = 1, stat) {
  df = proj$space
  df$x = proj$summary[[par]][,col,stat]
  plot_map(proj) +
    geom_point(data = df, aes(x = .data$lon, y = .data$lat, color = .data$x), size = 1.5) +
    scale_color_viridis_c(option = "turbo") +
    labs(title = paste0(par, "[,",col,"]", " posterior ", stat))
}


#' Title
#'
#' @param proj .
#' @param stat .
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @importFrom stats var
plot_map_data = function(proj, stat) {
  stopifnot("stat must be group, mean or var" = stat %in% c("group", "mean", "var"))
  df =
    proj$data$x |>
    dplyr::group_by_at("row") |>
    dplyr::summarise(
      mean = mean(value),
      var = stats::var(value)
    ) |>
    dplyr::mutate(
      group = unique(proj$data$x[,c("row", "group")]) |> {\(.) .$group}() |> factor()
    ) |>
    dplyr::left_join(proj$space, by = "row")


  plot_map(proj) +
    geom_point(data = df, aes(x = .data$lon, y = .data$lat, color = .data[[stat]]), size = 1.5) +
    {
      if (stat == "group") scale_color_viridis_d(option = "B") else scale_color_viridis_c(option = "turbo")
    } +
    labs(title = paste0("data rowise ", stat))
}
