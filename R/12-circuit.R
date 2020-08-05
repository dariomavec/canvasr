#' Canvas 12
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas12 <- function(
  width = 300,
  height = 500,
  bg_color = '#ffffff',
  accent_color = '#933A16',
  file_name = '12-circuit',
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
             width = width, height = height, bg_color = bgColor) +
    geom_point()

  save_plot(plot, file_name, width = width, height = height, ...)
}
