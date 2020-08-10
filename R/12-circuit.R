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
  bgColour = '#8E6E53',
  accentColour = c('#8E6E53', '#006E53'),
  file_name = '12-circuit',
  ...
  )
{
  plot_centre <- c(0, 0)
  n_walkers <- 10
  step_size <- 25
  steps <- 3
  sample_steps <- step_size * seq_len(steps)

  grid <- tibble(
    x0 = step_size * sample((-width / 2 / step_size):(width / 2 / step_size), n_walkers),
    y0 = rep(height/2, n_walkers),
    id = 1:n_walkers
  ) %>%
    mutate(walks = map2(x0, y0, function(x0, y0) {
      out <- tibble(x = x0, y = y0)
      while(y0 > -height / 2) {
        # down
        y0 <- y0 - sample(sample_steps, 1)
        out <- bind_rows(out, tibble(x = x0, y = y0))
        # right
        x0 <- x0 + sample(sample_steps, 1)
        out <- bind_rows(out, tibble(x = x0, y = y0))
        # down
        y0 <- y0 - sample(sample_steps, 1)
        out <- bind_rows(out, tibble(x = x0, y = y0))
        # left
        x0 <- x0 - sample(sample_steps, 1)
        out <- bind_rows(out, tibble(x = x0, y = y0))
      }
      out1 <- out
      out1$size <- 8
      out1$z <- 0

      out2 <- out
      out2$size <- 7
      out2$z <- 100
      bind_rows(out1, out2)
    })) %>%
    unnest(walks)

  plot <- grid %>%
    ggcanvas(aes(x, y, group = paste0(id, z)),
             width = width, height = height, bgColour = bgColour) +
    geom_path(aes(size = size, colour = factor(z)),
              lineend = 'square',
              linejoin = 'mitre') +
    scale_colour_manual(values = accentColour)

  save_plot(plot, file_name, width = width, height = height, ...)
}

# canvas12()
