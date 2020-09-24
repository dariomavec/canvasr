#' Canvas 11
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas11 <- function(
  width = 300,
  height = 500,
  squareHeight = 150,
  lineSize = 1,
  nRows = 6,
  nCols = 4,
  bgColor = '#ffffff',
  lineColor = '#933A16',
  fillColor = 'gray90',
  fileName = '11-breton',
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = seq(-width/2, width/2, length.out = nCols),
    y = seq(height/2, -height/2, length.out = nRows)
  ) %>%
    mutate(
      theta = runif(n(), 0, 2 * pi),
      h = runif(n(), 3/4, 1) * squareHeight,
      squares = pmap(list(x, y, h), square),
      squares = map2(squares, theta, rotate),
      squares = map(squares, split_poly, side1 = 2, side2 = 4)
    ) %>%
    unnest(squares) %>%
    mutate(
      squares = map(squares, split_poly, side1 = 2, side2 = 4)
    ) %>%
    unnest(squares) %>%
    mutate(
      squares = map(squares, split_poly, side1 = 2, side2 = 4)
    ) %>%
    unnest(squares) %>%
    mutate(
      id = 1:n(),
      points = map(squares, 'pts')) %>%
    select(id, points) %>%
    unnest(points)

  plot <- grid %>%
    ggcanvas(aes(x, y),
      width = width, height = height, bgColour = bgColor) +
    geom_polygon(aes(group = id),
                 fill = fillColor,
                 colour = lineColor,
                 size = lineSize)

  save_plot(plot, fileName, height = height, width = width, ...)
}

# canvas11()
