#' Canvas 2
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas_template <- function(
  width = 300,
  height = 500,
  background_color = '#FDF5E6',
  square_color = '#933A16',
  file_name = 'canvas03',
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = seq(0, width, by = squareHeight),
    y = seq(0, height, by = squareHeight)
  )

  plot <- grid %>%
    ggplot() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color),
      panel.grid = element_blank(),
      panel.border = element_blank()
      ) +
    coord_equal(xlim = c(-width/2, width/2),
                ylim = c(-height/2, height/2),
                expand = FALSE)

  save_plot(plot, file_name, height = height, width = width, ...)
}
