#' Canvas 15
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas15 <- function(
  width = 300,
  height = 500,
  bgColour = '#2E2E3A',
  accentColours = c("#97ead2","#8cc7a1","#816e94","#74226c","#4b2142", "#00A6ED"),
  fileName = '15-meteor',
  meteorN = 50,
  meteorWidths = c(50, 80, 120, 150),
  meteorHeights = c(0.5, 1, 1.5, 2),
  meteorRotation = -pi/2.8,
  meteorGradientSplits = 4,
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- tibble(
    x0 = runif(meteorN, -width/2, width/2),
    y0 = runif(meteorN, -height/2, height/2),
    w = sample(meteorWidths, meteorN, replace = TRUE),
    h = sample(meteorHeights, meteorN, replace = TRUE)
  ) %>%
    mutate(rects = pmap(list(x0, y0, w, h), rectangle),
           rects = modify(rects, rotate, theta = meteorRotation),
           rects = map(rects, split_poly_multiple, n = meteorGradientSplits),
           meteorId = 1:n(),
           fillId = rep_len(accentColours, n())) %>%
    unnest(rects) %>%
    group_by(meteorId) %>%
    mutate(alpha = ((1:n()) ** 4) / n() ** 4) %>%
    ungroup() %>%
    mutate(polygonId = 1:n(),
           pts = map(rects, 'pts')) %>%
    select(-rects) %>%
    unnest(pts)

  plot <- grid %>%
    ggcanvas(aes(x, y, group = polygonId, fill = fillId, alpha = alpha),
             width = width, height = height, bgColour = bgColour) +
    scale_fill_manual(values = accentColours) +
    geom_polygon()

  save_plot(plot, fileName, width = width, height = height, ...)
}

# canvas15()
