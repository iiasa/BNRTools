#' Expand an extent by a certain number
#'
#' @param e A object returned [`terra::ext`] object.
#' @param f [`numeric`] value to increase the extent (Default: \code{0.1}).
#'
#' @return Returns the unified total [`terra::ext`] object.
#'
#' @keywords utils
#' @examples
#' require(rast)
#' # Dummy raster
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- terra::rast(f)
#' (e <- terra::ext(r))
#'
#' # Now grow by a factor
#' (e2 <- spl_growExtent(e, f = 0.1))
#'
#' @author Martin Jung
spl_growExtent <- function(e,f=0.1){
  assertthat::assert_that(inherits(e,'SpatExtent'),
                          is.numeric(f))
  # Skip if f is 0
  if(f == 0) return(e)

  # Convert to vector
  e <- as.vector(e)
  xi <- (e['xmax']-e['xmin'])*(f/2)
  yi <- (e['ymax']-e['ymin'])*(f/2)

  xmin <- e['xmin']-xi
  xmax <- e['xmax']+xi
  ymin <- e['ymin']-yi
  ymax <- e['ymax']+yi

  return(terra::ext(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)))
}
