#' Canvas 5
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas05 <- function(
  width = 300,
  height = 500,
  bgColour = '#e62739',
  fileName = '05-charles',
  gridSpacing = 20,
  circleRadius = gridSpacing / 2,
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = seq(-width/2, width/2, by = gridSpacing * 2),
    y = seq(-height/2, height/2, by = gridSpacing * 2),
    r = circleRadius,
    r0 = 0
  )

  grid <- grid %>%
    bind_rows(
      mutate(grid,
             x = x + gridSpacing,
             y = y + gridSpacing)
    )

  diamond_cutout <- square(0, 0, 113) %>%
    rotate(pi/4) %>%
    .$pts

  # n_cutouts <- 3
  # cutouts_1 <- grid %>%
  #   slice_sample(n = 15) %>%
  #   mutate(x = x + gridSpacing / 2,
  #          y = y - gridSpacing / 2) %>%
  #   bind_rows(slice_sample(grid, n = 15))

  plot <- ggcanvas(width = width, height = height, bgColour = bgColour) +
    geom_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = r, start = 3 * pi / 4, end = 7 * pi / 4),
                 data = grid,
                 size = 0,
                 fill = '#9068be') +
    geom_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = r, start = -1 * pi / 4, end = 3 * pi / 4),
                 data = mutate(grid, x = x + gridSpacing/2, y = y - gridSpacing/2),
                 size = 0,
                 fill = '#6ed3cf') +
    geom_polygon(aes(x, y),
                 data = diamond_cutout,
                 fill = bgColour,
                 size = 0)
    # geom_circle(aes(x0=x, y0=y, r=r),
    #             data = cutouts_1,
    #             fill = bgColour,
    #             size = 0) +
    # geom_circle(aes(x0=x, y0=y, r=r),
    #             data = cutouts_2,
    #             fill = '#6ed3cf',
    #             colour = '#9068be',
    #             size = 0.5)  +

  save_plot(plot, fileName, height = height, width = width, ...)
}

canvas05()
