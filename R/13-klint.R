#' Canvas 13
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas13 <- function(
  width = 300,
  height = 500,
  bgColour = '#885053',
  accentColour = c('#777DA7', '#FDFFFC', '#D19C1D', '#2A2B2A', '#8E3B46'),
  fileName = '13-klint',
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- tibble(x0 = rep(0, 5),
                 y0 = rep(0, 5),
                 r0 = rep(0, 5),
                 r = c(5, 5, 3, 3, 1) * 20,
                 fill = letters[1:5],
                 start = 0,
                 end = c(pi, -pi, pi, -pi, pi)
                 )

  plot <- grid %>%
    ggcanvas(width = width, height = height, bgColour = bgColour) +
    geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r  = r,
                     start = start, end = end, fill = fill),
                 size = 0) +
    scale_fill_manual(values = accentColour)

  save_plot(plot, fileName, width = width, height = height, ...)
}

# canvas13()
