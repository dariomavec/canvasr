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
  h = 100,
  bg_color = '#000000',
  colors = c("#6565a4","#cdedfd","#b6dcfe","#a9f8fb","#31999b"),
  file_name = 'canvas07',
  ...
  )
{
  plot_centre <- c(0, 0)

  x <- seq(-width/2, width/2, by = h)
  y <- seq(-height/2, height/2, by = h)

  output <- list()
  for (i in x) {
    for (j in y) {
      s <- list(square(i, j, h))
      m <- sample(c(-1, 1), 1)
      r <- runif(1)

      if (r < 0) {
        s <- map(s, bisect_square, m = m) %>%
          flatten()
      }
      else if (r < 4) {
        s <- map(s, split_square) %>%
          flatten() %>%
          map(
           ~{
             if (runif(1) < 0.25) {
               m <- sample(c(-1, 1), 1)
               bisect_square(.x, m) %>%
                 flatten()
             }
             else .x
           }
          )
        }
      }
      output <- append(output, s)
    }

  # grid <- expand_grid(
  #   x = seq(-width/2, width/2, by = h),
  #   y = seq(-height/2, height/2, by = h)
  # ) %>%
  #   mutate(s = map2(x, y, square, h = h),
  #          m = sample(c(-1, 1), n(), replace = T),
  #          r = runif(n()),
  #          s = pmap(list(s, m, r), function(s, m, r) {
  #            if (r < 0.25) bisect_square(s, m)
  #            else if (r < 0.4) map2(split_square(s),
  #                                   sample(c(-1,1), 4, replace = T),
  #                                   bisect_square) %>%
  #              flatten()
  #            else list(s)
  #          })) %>%
  #   unnest(s) %>%
  #   mutate(id = 1:n(),
  #          coords = map(s, 'pts'),
  #          x0 = map_dbl(s, 'x'),
  #          y0 = map_dbl(s, 'y'),
  #          fill = sample(colors, n(), replace = T)) %>%
  #   select(-x, -y, -s) %>%
  #   unnest(coords)

  plot <- grid %>%
    ggplot() +
    geom_polygon(aes(x, y, group = id, fill = fill), size=0) +
    # geom_point(aes(x0, y0)) +
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
