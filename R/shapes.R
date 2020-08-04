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
      y = y,
      h = h
    ),
    class = c('square', 'poly')
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

split_square <- function(s) {
  list(
    square(s$x - s$h/4, s$y - s$h/4, s$h/2),
    square(s$x - s$h/4, s$y + s$h/4, s$h/2),
    square(s$x + s$h/4, s$y + s$h/4, s$h/2),
    square(s$x + s$h/4, s$y - s$h/4, s$h/2)
  )
}

split_poly <- function(poly, side1, side2, p1=0.5, p2=0.5) {
  stopifnot(0 < side1 & side1 < side2)
  stopifnot(side2 < nrow(poly$pts))
  stopifnot(0 <= p1 & p1 <= 1)
  stopifnot(0 <= p2 & p2 <= 1)

  poly$h <- NULL
  poly1 <- poly2 <- poly

  new_pts <- list(
    (1 - p1) * poly$pts[side1,] + p1 * poly$pts[(side1 + 1),],
    (1 - p2) * poly$pts[side2,] + p2 * poly$pts[(side2 + 1),]
  )

  poly1$pts <- bind_rows(
    poly$pts[1:side1,],
    new_pts[[1]],
    new_pts[[2]],
    poly$pts[(side2+1):nrow(poly$pts),],
  )
  poly1 <- update_centroid(poly1)

  poly2$pts <- bind_rows(
    new_pts[[2]],
    new_pts[[1]],
    poly$pts[(side1 + 1):side2,],
    new_pts[[2]]
  )
  poly2 <- update_centroid(poly2)

  return(list(poly1, poly2))
}

update_centroid <- function(poly) {
  x <- poly$pts$x[-1]
  y <- poly$pts$y[-1]

  centroid_x <- centroid_y <- determinant <- 0
  for (i in 1:length(x)) {
    if (i == length(x)) {
      j <- 1
    } else {
      j <- i + 1
    }

    vertex_determinant <- x[i] * y[j] - x[j] * y[i]
    determinant <- determinant + vertex_determinant

    centroid_x <- centroid_x + (x[i] + x[j]) * vertex_determinant
    centroid_y <- centroid_y + (y[i] + y[j]) * vertex_determinant
  }

  centroid <- c(centroid_x, centroid_y) / (3 * determinant)
  poly$x <- centroid[1]
  poly$y <- centroid[2]

  return(poly)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.poly <- function(x, ...) {
  cat(glue("poly: ({x$x}, {x$y})"))
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.triangle <- function(x, ...) {
  cat(glue("triangle: ({x$x}, {x$y}, {x$h})"))
}

split_squares_deep <- function(square, depth = 1) {
  square <- split_square(square)

  if (depth >= 1) {
    square <- map(square, split_squares_deep, depth = depth - 1) %>%
      flatten()
  }
  return(square)
}
