#' Canvas _
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
  bgColour = '#ffffff',
  accentColour = '#933A16',
  fileName = '___',
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = seq(-width/2, width/2, by = width / 10),
    y = seq(-height/2, height/2, by = height / 10)
  )

  plot <- grid %>%
    ggcanvas(aes(x, y),
             width = width, height = height, bgColour = bgColour) +
    geom_point()

  save_plot(plot, fileName, width = width, height = height, ...)
}
