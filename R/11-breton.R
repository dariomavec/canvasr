#' Canvas 10
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
  bg_color = '#ffffff',
  accent_color = '#933A16',
  file_name = '11-breton',
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    x = seq(-width/2, width/2, by = width / 10),
    y = seq(-height/2, height/2, by = height / 10)
  )

  plot <- grid %>%
    ggplot() +
    theme_canvasr(width, height, bg_color)

  save_plot(plot, file_name, height = height, width = width, ...)
}

# canvas11()
