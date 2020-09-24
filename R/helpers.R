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
#' @import animation
save_vid <- function(
  file_name,
  path = 'output',
  output_type = 'mp4',
  repeat_last = 12,
  fps = 4,
  optimize = TRUE) {
  files <- list.files(path=glue("{path}/{file_name}/"), pattern = '*.png', full.names = TRUE)
  final_img <- image_read(last(files)) %>%
    image_info()

  dims <- list(
    width = round(final_img$width / 2, 0),
    height = round(final_img$height / 2, 0)
  )
  final_geometry <- paste0(dims$width, 'x', dims$height)

  imgs <- c(image_read(last(files)), image_read(files)
  ) %>%
    # image_join() %>% # joins image
    image_apply(function(x) x %>%
                  image_resize(final_geometry) %>%
                  image_extent(final_geometry, color = '#fff'))

  if (output_type == 'mp4') {
    saveVideo({
      walk(1:length(imgs), ~{
        par(mar = rep(0, 4))
        plot(as.raster(imgs[.x]))
      })
    },
    video.name = glue("{path}/{file_name}.mp4"),
    ani.width = dims$width + if(dims$width %% 2 == 1) 1 else 0,
    ani.height = dims$height + if(dims$height %% 2 == 1) 1 else 0,
    interval = 1 / fps)
  } else if (output_type == 'gif') {
    c(rep(imgs[1], repeat_last), imgs[-1]) %>%
      image_animate(fps=fps, optimize = optimize) %>% # animates, can opt for number of loops
      image_write(glue("{path}/{file_name}.gif"))
  }
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
