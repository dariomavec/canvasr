#' Canvas 6
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas06 <- function(
  width = 300,
  height = 500,
  bg_color = '#ffffff',
  accent_color = '#933A16',
  file_name = 'canvas06',
  colors = c("#beef9e","#a6c36f","#828c51","#335145","#1e352f"),
  ...
  )
{
  plot_centre <- c(0, 0)
  squareHeight <- 50

  grid <- expand_grid(
    x = seq(-(width + squareHeight) / 2, (width + squareHeight) / 2, by = squareHeight),
    y = seq(-(height + squareHeight) / 2, (height + squareHeight) / 2, by = squareHeight)
  ) %>%
    mutate(fill1 = sample(colors, n(), replace = TRUE),
           fill2 = sample(colors, n(), replace = TRUE),
           fill3 = sample(colors, n(), replace = TRUE),
           r2 = floor(runif(n(), 15, 25)),
           r3 = floor(runif(n(), 5, 10))
    )

  plot <- grid %>%
    ggplot() +
    geom_rect(aes(xmin = x - squareHeight/2, xmax = x + squareHeight/2,
                  ymin = y - squareHeight/2, ymax = y + squareHeight/2,
                  fill = fill1)) +
    geom_circle(aes(x0 = x, y0 = y, r = r2, fill = fill2), size = 0) +
    geom_circle(aes(x0 = x, y0 = y, r = r3, fill = fill3), size = 0) +
    scale_fill_manual(values = colors) +
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

# canvas06()
