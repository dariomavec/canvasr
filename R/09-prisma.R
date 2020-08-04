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
  bg_color = '#29274C',
  accent_color = '#933A16',
  # line_colors = c("#f4e409","#eeba0b","#c36f09","#a63c06","#710000"),
  line_colors = rep(c("#337ca0","#3ec300","#fffc31","#ff1d15","#e13700"), 50),
  file_name = '09-prisma',
  strips = 15,
  r = 50,
  n = 60,
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
    ggplot(aes(x, y, group = id, colour = colour)) +
    geom_path() +
    scale_colour_manual(values = line_colors) +
    theme_void() +
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

# canvas08()
