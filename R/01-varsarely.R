#' Canvas 1
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas01 <- function(
  width = 300,
  height = 500,
  squareHeight = 10,
  gutter = 10,
  background_color = '#FDF5E6',
  square_color = '#933A16',
  square_color_shift = 'gray40',
  file_name = '01-varsarely',
  ...
  )
{
  plot_centre <- c(width / 2, height / 2)

  grid <- expand_grid(
    x = seq(0, width, by = squareHeight),
    y = seq(0, height, by = squareHeight)
  ) %>%
    transmute(
      id = paste0(x, '-', y),
      # likelihood = sqrt((plot_centre[1] - x) ** 2 +
      #                   (plot_centre[2] - y) ** 2),
      # rotate = runif(n()) > (likelihood / max(likelihood)),
      r = runif(n()) < 0.05,
      square = map2(x, y, square, h = squareHeight - gutter / 2),
      square = modify_if(square, r, rotate, theta = pi/12),
      coords = map(square, 'pts')
    ) %>%
    select(-square) %>%
    unnest(coords)

  plot <- grid %>%
    ggplot(aes(x, y, group = id)) +
    geom_polygon(aes(fill = r), show.legend = FALSE) +
    scale_fill_manual(values = c(square_color, square_color_shift)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color),
      panel.grid = element_blank(),
      panel.border = element_blank()
      ) +
    coord_equal()

  save_plot(plot, file_name, height = height, width = width)
}
