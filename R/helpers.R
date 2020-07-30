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
    image_read() %>%
    image_trim()

  image_write(fig, output_path)
  image_write(fig, temp_path)
}

#' @import magick
save_gif <- function(
  file_name,
  path = 'output') {
  files <- list.files(path=glue("{path}/{file_name}/"), pattern = '*.png', full.names = TRUE)
  files <- c(rep(last(files), 12), files)

  files %>%
    image_read() %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=5) %>% # animates, can opt for number of loops
    image_write(glue("{path}/{file_name}.gif"))
}

rotate_poly <- function(poly, origin, theta = -pi / 10) {
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta), cos(theta)),
              nrow = 2)

  map(poly, function(xy) {
    rotated <- R %*% (xy - origin)
    rotated[1:2, 1] + origin
  })
}

square_from_centre <- function(origin, height, rotate = FALSE, theta = -pi / 10) {
  xmin = origin[1] - height / 2
  xmax = origin[1] + height / 2
  ymin = origin[2] - height / 2
  ymax = origin[2] + height / 2

  square <- list(
    c(xmin, ymin),
    c(xmin, ymax),
    c(xmax, ymax),
    c(xmax, ymin),
    c(xmin, ymin)
  )

  if (rotate) square <- rotate_poly(square, origin, theta)

  return(square)
}
