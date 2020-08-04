#' Canvas 9
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas09 <- function(
  width = 300,
  height = 500,
  bg_color = '#ffffff',
  accent_color = '#933A16',
  file_name = 'canvas09',
  steps = 40,
    ...
)
{
  plot_centre <- c(0, 0)
  colours <- rgb(200, seq(0, 167, length.out = steps), seq(100, 0, length.out = steps), maxColorValue = 255)

  grid <- tibble(
    x = 0,
    y = 0,
    theta = seq(pi/2, 2.5 * pi, length.out = steps)
  ) %>%
    mutate(
      points = pmap(list(x, y, theta), function(x, y, theta) {
        h <- height
        w <- width
        t <- 2 * pi / (steps-1) + 1e-2

        list(
          c(x, y),
          c(x + w * cos(theta), y + h * sin(theta)),
          c(x + w * cos(theta - t), y + h * sin(theta - t)),
          c(x, y)
        )
      })
    ) %>%
    unnest(points)

  plot <- grid %>%
    ggplot(aes(x = map_dbl(points, 1),
               y = map_dbl(points, 2),
               fill = factor(theta))) +
    geom_polygon() +
    scale_fill_manual(values = colours) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = bg_color),
      plot.background = element_rect(fill = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none'
    ) +
    coord_equal(xlim = c(-width/2, width/2),
                ylim = c(-height/2, height/2),
                expand = FALSE)

  save_plot(plot, file_name, height = height, width = width, ...)
}

canvas09()
