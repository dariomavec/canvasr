#' Canvas 16
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas16 <- function(
  width = 300,
  height = 500,
  bgColour = '#ffffff',
  accentColours = c("#50514f","#f25f5c","#ffe066","#247ba0","#70c1b3","#8e9dcc","#d9dbf1","#f3d9dc"),
  # accentColours = c("#94690c","#8c826d","#8292b3","#d4ba83","#a2ebd4","#6e6568","#359476","#fcb10d"),
  fileName = '16-boxer',
  colMinWidth = 10,
  colMaxWidth = 50,
  rowMinHeight = 20,
  rowMaxHeight = 60,
  ...
  )
{
  plot_centre <- c(0, 0)

  w <- vector('numeric')
  while(sum(w) < width) {
    w <- c(w, runif(1, colMinWidth, colMaxWidth))
  }

  grid <- tibble(
    w = w
  ) %>%
    mutate(x0 = cumsum(w) - width/2 - w/2,
           h = map(x0, ~{
             temp_h <- vector('numeric')
             while(sum(temp_h) < height) {
               temp_h <- c(temp_h, runif(1, rowMinHeight, rowMaxHeight))
             }
             temp_h
           })) %>%
    unnest(h) %>%
    group_by(x0) %>%
    mutate(y0 = cumsum(h) - height/2 - h/2) %>%
    ungroup() %>%
    mutate(
      rects = pmap(list(x0, y0, w, h), rectangle),
      pts = map(rects, 'pts'),
      id = 1:n()) %>%
    unnest(pts)

  n_boxes <- max(grid$id)

  plot <- grid %>%
    ggcanvas(aes(x, y, group = id, fill = factor(id)),
             width = width, height = height, bgColour = bgColour) +
    geom_polygon(size = 0) +
    scale_fill_manual(values = sample(accentColours, n_boxes, replace = TRUE))

  save_plot(plot, fileName, width = width, height = height, ...)
}

# canvas16()
