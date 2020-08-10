#' Canvas 9
#'
#' @return
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @importFrom glue glue
canvas09 <- function(
  width = 300,
  height = 500,
  bgColour = '#29274C',
  accent_color = '#933A16',
  # line_colors = c("#f4e409","#eeba0b","#c36f09","#a63c06","#710000"),
  line_colors = rep(c("#337ca0","#3ec300","#fffc31","#ff1d15","#e13700"), 50),
  file_name = '09-prisma',
  strips = 10,
  r = 50,
  n = 150,
  ...
  )
{
  plot_centre <- c(0, 0)
  line_df <- tibble(x = seq(-.99, .99, length = strips))

  grid <- tibble(
    x0 = width * runif(n, -1/2, 1/2),
    y0 = height * runif(n, -1/2, 1/2),
    radius = r * runif(n, 0.5, 1)
  ) %>%
    full_join(line_df, by = character()) %>%
    mutate(
      y = pmap(list(x, y0, radius), function(x, y0, radius) {
        c(y0 + radius * sin(acos(x)), y0 - radius * sin(acos(x)))
      }),
      x = radius * x + x0,
      id = 1:n(),
      colour = paste0(x0, y0)
    ) %>%
    unnest(y) %>%
    select(id, x, y, colour)

  plot <- grid %>%
    ggcanvas(aes(x, y, group = id, colour = colour),
             width = width, height = height, bgColour = bgColour) +
    geom_path() +
    scale_colour_manual(values = line_colors)

  save_plot(plot, file_name, height = height, width = width, ...)
}

# canvas09()
