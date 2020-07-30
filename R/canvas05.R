#' Canvas 5
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas05 <- function(
  width = 300,
  height = 500,
  bg_color = '#06060a',
  accent_color = '#c49102',
  planet_color = '#c43235',
  file_name = 'canvas05',
  n_circles = 6,
  # shrinkage = 15,
  a0 = 120,
  a1 = 40,
  b0 = 6,
  b1 = 9,
  top = height / 3,
  bottom = -height / 2,
  ...
  )
{
  plot_centre <- c(0, 0)

  grid <- expand_grid(
    i = 0:n_circles,
    x0 = 0) %>%
    mutate(
      y0 = -height / 2 + i * (top - bottom) / n_circles,
      a = a0 + (a1 - a0) * i / n_circles,
      b = b0 + (b1 - b0) * i / n_circles
    )

  partial_ellipse <- function(x0, y0, a, b, start, end, ncp = 100) {
    map_dfr(0:ncp, function(i) {
      theta <- start + i * (end - start) / ncp

      list(x = x0 + a * cos(theta),
           y = y0 + b * sin(theta))
    })
  }
  front_ellipse <- partial_ellipse(
    x0 = 0,
    y0 = top,
    a = a1,
    b = b1,
    start = 0,
    end = pi
  )

  g <- make_gradient(
    deg = 90, n = 500, cols = c('#4f8a8b', bg_color, bg_color)
  )

  plot <- ggplot() +
    annotation_custom(
      grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    geom_path(aes(x = c(0, 0),
                  y = c(bottom, top)),
              color = 'gray40', linetype = 3) +
    geom_ellipse(aes(x0=x0, y0=y0, a=a, b=b, angle = 0),
                 data = grid,
                 # fill = 'white',
                 color = accent_color) +
    geom_circle(aes(x0 = 0, y0 = top, r = 27),
                size = 0,
                fill = planet_color) +
    geom_path(aes(x, y), data = front_ellipse,
              color = accent_color) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = bg_color),
      plot.background = element_rect(fill = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank()
      ) +
    coord_equal(xlim = c(-width/2, width/2),
                ylim = c(-height/2, height/2),
                expand = FALSE)

  save_plot(plot, file_name, height = height, width = width, ...)
}

# canvas05()
