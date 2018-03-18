#' Just map it
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
jmi <- function(x, ... ) {
  if (missing(x)) x <- NULL
  UseMethod("jmi", object = x)
}

process_args <- function(x, ...) {

}
#' @name jmi
#' @export
jmi.default <- function(x = NULL, ...) {
  l <- list(...)
  if (is.null(l$add)) l$add <- FALSE
  forward <-NULL
  if (is.null(x)) {
    x <- cbind(world$x, world$y)
  }
  if (!l$add) {
    options(jmi.last.proj = raster::projection(x))
  } else {
    forward <- getOption("jmi.last.proj")
  }


  if (!is.null(forward)) x<- proj4::ptransform(x * pi/180, src.proj = "+init=epsg:4326", forward)
  if (l$add) do.call(lines, l) else do.call(plot, l)
  invisible(NULL)
}
#' @name jmi
#' @export
jmi.BasicRaster <- function(x = NULL, ...) {
  l <- list(...)
  if (is.null(l$add)) l$add <- FALSE
  forward <-NULL
  if (!l$add) {
    options(jmi.last.proj = raster::projection(x))
  } else {
    forward <- getOption("jmi.last.proj")
  }


  if (!is.null(forward)) x <- projectRaster(x, crs = forward)
  l$x <- x
  do.call(plot, l)
  invisible(NULL)
}
#' @name jmi
#' @export
jmi.Spatial <- function(x = NULL, ...) {
  l <- list(...)
  if (is.null(l$add)) l$add <- FALSE
  forward <-NULL
  if (!l$add) {
    options(jmi.last.proj = raster::projection(x))
  } else {
    forward <- getOption("jmi.last.proj")
  }
  if (!is.null(forward)) x <- sp::spTransform(x, sp::CRS(forward))
  l$x <- x
  do.call(plot, l)
  invisible(NULL)

}


