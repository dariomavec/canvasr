#' Save Plot
#'
#' @param plot
#' @param file_name
#' @param width
#' @param height
#' @param path
#'
#' @return
#' @export
#' @import ggplot2
save_plot <- function(
  plot,
  file_name,
  width,
  height,
  path = 'output',
  echo = TRUE,
  trim = TRUE,
  ...) {
  dir.create(glue("{path}/{file_name}/"), showWarnings = FALSE)
  temp_path <- glue("{path}/{file_name}/{format(Sys.time(), '%y-%m-%d-%H-%M-%S')}.png")
  output_path <- glue("{path}/{file_name}.png")

  # fig <- image_graph(width = width,
  #                    height = height,
  #                    res = 96)
  if (echo) print(plot)

  ggplot2::ggsave(plot,
                  filename = output_path,
                  width = width / 100,
                  height = height / 100,
                  device = 'png')

  fig <- output_path %>%
    image_read()

  if (trim) fig <- fig %>% image_trim()

  image_write(fig, output_path)
  image_write(fig, temp_path)
}

#' @import magick
save_gif <- function(
  file_name,
  path = 'output',
  repeat_last = 12,
  fps = 4,
  optimize = TRUE) {
  files <- list.files(path=glue("{path}/{file_name}/"), pattern = '*.png', full.names = TRUE)
  files <- c(rep(last(files), repeat_last), files)
  final_img <- image_read(files[1]) %>%
    image_info()
  if (optimize) {
    final_geometry <- paste0(round(final_img$width / 3, 0), 'x',
                             round(final_img$height / 3, 0))#, '!"')
  }

  files %>%
    image_read() %>% # reads each path file
    image_join() %>% # joins image
    image_apply(function(x) x %>%
                  image_resize(final_geometry) %>%
                  image_extent(final_geometry)) %>%
    image_animate(fps=fps, optimize = optimize) %>% # animates, can opt for number of loops
    image_write(glue("{path}/{file_name}.gif"))
}

#' Make Gradient
#'
#' @param deg
#' @param n
#' @param cols
#'
#' @return
#' @export
#' @import grid
#' @import RColorBrewer
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
}

ggcanvas <- function(..., width, height, bgColour) {
  ggplot(...) +
    coord_equal(xlim = c(-width/2, width/2),
                ylim = c(-height/2, height/2),
                expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = bgColour),
      plot.background = element_rect(fill = bgColour),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none'
    )
}
