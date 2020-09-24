#' Canvas 25
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas25 <- function(
  width = 300,
  height = 500,
  bgColour = '#F3F9E3',
  accentColours = c("#eacbd2","#dfaeb4","#dd9ac2","#b486ab"),#,"#82667f","#703d57","#402a2c"),
  fileName = '25-shortcake',
  ...
  )
{
  plot_centre <- c(0, 0)
  phi <- (1 + sqrt(5)) / 2 - 1
  goldenAngle <- 2 * pi * phi
  maxRadius <- 20
  minRadius <- 1.5
  maxAlpha <- 1
  minAlpha <- 0.5

  grid <- tibble(
    id = 1:300
  ) %>%
    mutate(x = id * cos(goldenAngle * id),
           y = id * sin(goldenAngle * id),
           r = minRadius + (maxRadius - minRadius) * id / max(id),
           alpha = minAlpha + (maxAlpha - minAlpha) * id / max(id))

  plot <- grid %>%
    ggcanvas(aes(x0 = x,
                 y0 = y,
                 r = r,
                 alpha = alpha,
                 fill = factor(id %% length(accentColours))),
             width = width, height = height, bgColour = bgColour) +
    scale_fill_manual(values = accentColours) +
    geom_circle(size = 0)

  save_plot(plot, fileName, width = width, height = height, ...)
}

# canvas25()
