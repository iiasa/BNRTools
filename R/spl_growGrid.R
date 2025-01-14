#' Grow a categorical SpatRaster by certain amount of pixels or distance.
#'
#' @description
#' A common issue in aggregating categorical rasters is that coastal and boundary
#' gridcells tend to decrease owing to aggregation effects. This can cause issues
#' of non-matching grid cells later on.
#' This simple function takes a categorical [`SpatRaster`] object and grows it
#' into no-data areas (those with \code{NA} values) within an optionally provided
#' distance.
#'
#' @note
#' Only \code{NA} grid cells will be filled!
#'
#' @param x A categorical [`SpatRaster`] to be grown.
#' @param iter A [`numeric`] values of the maximum number of iterations from the grid
#' cell border of which \code{x} should be grown (Default: \code{2}).
#'
#' @returns [`SpatRaster`]
#'
#' @keywords spatial
#'
#' @seealso
#' \code{\link[terra]{buffer}},
#' \code{\link[terra]{focal}}
#'
#' @examples
#' set.seed(42)
#' ras <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
#' ymin = 0, ymax = 100, resolution = 10, crs = NA)
#'
#' # Fill with dummy values but keep half as NA
#' terra::values(ras) <- c(rep(NA,50),rbinom(terra::ncell(ras)/2, 10, 0.5))
#'
#' # Convert to factor
#' ras <- terra::as.factor(ras)
#' assertthat::assert_that(terra::is.factor(ras))
#'
#' ras_nona <- spl_growGrid(x = ras, iter = 10)
#' terra::plot(ras_nona)
#'
#' @export
spl_growGrid <- function(x, iter = 2) {

  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    is.numeric(iter) && iter > 0
  )

  # Check if there is any NA, otherwise return
  if(!terra::global(x, "anyNA")[,1]) return(x)

  # Save categories
  if(terra::is.factor(x)) cats <- terra::cats(x) else cats <- NULL

  # Create dummy and progress per iteration
  new <- x
  pb <- progress::progress_bar$new(total = iter)

  for(i in 1:iter){
    pb$tick()

    # Check if no-data is still present. If no, skip
    if(!terra::global(new, "anyNA")[,1]) next()

    # Buffer
    new <- terra::focal(x = new,
                      w = 3,
                      fun = "modal",
                      na.policy = "only"
    )
  }

  # Convert to factor again
  if(terra::is.factor(x)) new <- terra::as.factor(new)

  # Return output
  return(new)
}
