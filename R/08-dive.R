#' Canvas 7
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas07 <- function(
  width = 300,
  height = 500,
  h = 25,
  bg_color = '#000000',
  colors = c("#31999b","#cdedfd","#6565a4","#b6dcfe","#a9f8fb"),
  file_name = 'canvas07',
  depth = 3,
  r1 = 0.60,
  exp = 8,
  ...
  )
{
  plot_centre <- c(0, 0)

  x <- seq(-width/2, width/2, by = h)
  y <- seq(-height/2, height/2, by = h)

  split_diagonal <- function(poly, r1 = 0.25) {
    r <- runif(1)
    if (r > (1 - r1)) {
      split_type <- sample(c('ltr', 'rtl'), 1)
      if (split_type == 'ltr') {
        poly <- split_poly(poly, 1, 2, 0, 1)
      } else if (split_type == 'rtl') {
        poly <- split_poly(poly, 1, 3, 1, 1)
      }
      return(poly)
    }
    else return(list(poly))
  }

  grid <- expand_grid(
    x = seq(-width/2, width/2, by = h),
    y = seq(-height/2, height/2, by = h)
  ) %>%
    mutate(polys = map2(x, y, square, h = h),
           depth = depth * (1 - ((x**2 + y**2) / (max(x**2 + y**2))))**exp,
           depth = round(depth, 0),
           polys = map2(polys, depth, split_squares_deep)) %>%
    unnest(polys) %>%
    mutate(polys = map(polys, split_diagonal, r1 = r1)) %>%
    unnest(polys) %>%
    mutate(id = 1:n(),
           coords = map(polys, 'pts'),
           x0 = map_dbl(polys, 'x'),
           y0 = map_dbl(polys, 'y'),
           p_colour = 0.5 * (1 - ((x0**2 + y0**2) / (max(x0**2 + y0**2))))**exp,
           fill = map_chr(p_colour, ~sample(colors, 1,
                                            prob = c(.x, rep(0.25, 4))))
           ) %>%
    select(-x, -y, -polys) %>%
    unnest(coords)

  plot <- grid %>%
    ggplot() +
    geom_polygon(aes(x, y, group = id, fill = fill)) +#, colour = 'gray50', size = 0.1) +
    theme_void() +
    scale_fill_manual(values = colors) +
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

# canvas07()
