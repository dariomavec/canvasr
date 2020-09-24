#' Canvas 20
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import forcats
#' @importFrom glue glue
canvas20 <- function(
  width = 300,
  height = 500,
  bgColour = '#ffffff',
  accentColours = c("#995d81","#eb8258","#f6f740","#d8dc6a","#6689a1"),
  fileName = '20-melon',
  n = 20,
  r_min = 10,
  r_max = 30,
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = 0,
    y = 0,
    r = runif(n, r_min, r_max)
  ) %>%
    mutate(
      r = cumsum(r),
      hexes = pmap(list(x, y, r), hexagon),
      hexes = map(hexes, rotate, theta = pi/2),
      points = map(hexes, 'pts'),
      fill = sample(accentColours, n(), replace = TRUE),
      group = row_number(-r)
    ) %>%
    select(group, points, fill) %>%
    unnest(points)

  plot <- grid %>%
    ggcanvas(aes(x, y, group = group, fill = fill),
             width = width, height = height, bgColour = bgColour) +
    geom_polygon() +
    scale_fill_manual(values = accentColours)

  save_plot(plot, fileName, width = width, height = height, ...)
}

# canvas20()
