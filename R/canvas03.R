#' Canvas 3
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import ggforce
#' @importFrom glue glue
canvas03 <- function(
  width = 300,
  height = 600,
  background_color = '#8D8741',
  line_color = '#FBEEC1',
  file_name = 'canvas03',
  ...
  )
{
  plot_centre <- c(0, 0)

  circles <- expand_grid(
    x = c(-width/2, width/2), y = c(-height/2, height/2),
    r = 1:4 * 20
  ) %>%
    bind_rows(
      expand_grid(
        x = 0, y = 0,
        r = 1:4 * 20
      )
    )

  lines <- tibble(
    x = c(-4, -1, 1, 4) * 20,
    y = rep(0, 4),
    id = rep(1:2, each = 2)
  )

  diamond <- function(x, y, r) {
    list(
      c(x, y+r),
      c(x+r/2, y),
      c(x, y-r),
      c(x-r/2, y),
      c(x, y+r)
    )
  }

  diamonds <- expand_grid(
      x = 0, y = 0,
      r = 180 + 1:3 * 40
    ) %>%
    bind_rows(
      expand_grid(
        x = c(-width/2, width/2), y = 0,
        r = 300
      )
    ) %>%
    mutate(
      id = paste0(x, y, r),
      points = pmap(list(x, y, r), diamond)) %>%
    unnest(points)

  plot <- ggplot() +
    # geom_point(color = line_color) +
    geom_circle(aes(x0 = x, y0 = y, r = r), data = circles, color = line_color) +
    geom_path(aes(x = map_dbl(points, 1),
                     y = map_dbl(points, 2),
                     group = id),
                 data = diamonds,
                 color = line_color) +
    geom_path(aes(x, y, group = id), data = lines, color = line_color) +
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

# canvas03()
