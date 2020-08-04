#' Canvas 5
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas05 <- function(
  width = 300,
  height = 500,
  bg_color = '#e62739',
  file_name = '05-charles',
  ...
  )
{
  plot_centre <- c(0, 0)
  size <- 20

  grid <- expand_grid(
    x = seq(-width/2, width/2, by = size * 2),
    y = seq(-height/2, height/2, by = size * 2),
    r = size / 2,
    r0 = 0
  )
  grid <- bind_rows(grid,
                    mutate(grid,
                           x = x + size,
                           y = y + size))

  plot <- ggplot() +
    geom_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = r, start = 3 * pi / 4, end = 7 * pi / 4),
                 data = grid,
                 size = 0,
                 fill = '#9068be') +
    geom_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = r, start = -1 * pi / 4, end = 3 * pi / 4),
                 data = mutate(grid, x = x + size/2, y = y - size/2),
                 size = 0,
                 fill = '#6ed3cf') +
    theme_void() +
    theme(
      panel.background = element_rect(fill = bg_color),
      plot.background = element_rect(fill = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank()
      ) +
    coord_equal(xlim = c(-width/2, width/2),
                ylim = c(-height/2, height/2),
                expand = FALSE)

  save_plot(plot, file_name, height = height, width = width, ...)
}

# canvas04()
