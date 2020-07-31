rotate_poly <- function(poly, origin, theta = -pi / 10) {
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta), cos(theta)),
              nrow = 2)

  map(poly, function(xy) {
    rotated <- R %*% (xy - origin)
    rotated[1:2, 1] + origin
  })
}

new_square <- function(x, y, h) {
  stopifnot(typeof(x) == 'double')
  stopifnot(typeof(y) == 'double')
  stopifnot(typeof(h) == 'double')

  xmin <- x - h/2
  xmax <- x + h/2
  ymin <- y - h/2
  ymax <- y + h/2

  pts <- tribble(
    ~x, ~y,
    xmin, ymin,
    xmin, ymax,
    xmax, ymax,
    xmax, ymin,
    xmin, ymin
  )

  structure(
    list(
      pts = pts,
      x = x,
      y = y
    ),
    class = 'poly'
  )
}

square <- function(x, y, h) {
  new_square(x, y, h)
}

#' Title
#'
#' @param poly
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
rotate <- function(poly, theta) {
  UseMethod("rotate")
}

#' Title
#'
#' @param poly
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
rotate.default <- function(poly, theta) {
}

#' Title
#'
#' @param poly
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
rotate.poly <- function(poly, theta = -pi/4) {
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta), cos(theta)),
              nrow = 2)

  origin <- c(poly$x, poly$y)
  poly <- translate(poly, -origin[1], -origin[2])

  poly$pts <- as.matrix(poly$pts) %*% R %>%
    magrittr::set_colnames(c('x', 'y')) %>%
    as_tibble()

  poly <- translate(poly, origin[1], origin[2])

  return(poly)
}

#' Title
#'
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
translate <- function(poly, x, y) {
  UseMethod("translate")
}

#' Title
#'
#' @param poly
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
translate.poly <- function(poly, x, y) {
  poly$pts <- poly$pts %>%
    mutate(x = x + !!x,
           y = y + !!y)

  poly$x <- poly$x - x
  poly$x <- poly$x - y

  return(poly)
}
