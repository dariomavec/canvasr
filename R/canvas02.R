#' Canvas 2
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas2 <- function(
  width = 300,
  height = 500,
  theta = -pi / 48,
  scale = 1.15,
  n_steps = 60,
  background_color = '#db451f',
  file_name = 'canvas2',
  ...
  )
{
  plot_centre <- c(width / 2, height / 2)

  grid <- tibble(id = 0:n_steps) %>%
    mutate(
      polys = map(id,
                  ~{
                    square_from_centre(
                      origin = c(0,0),
                      height = (scale ** .x) * (height / 100),
                      rotate = TRUE,
                      theta = .x * theta
                    )
                  }
      )
    ) %>%
    unnest(polys) %>%
    rename(coords = polys)

  plot <- grid %>%
    ggplot(aes(x = map_dbl(coords, 1),
               y = map_dbl(coords, 2),
               group = factor(id))) +
    geom_path(colour = 'white') +
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

# canvas2()
