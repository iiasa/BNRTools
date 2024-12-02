#' Resample raster
#'
#' @description
#' Resample two raster to the spatial resolution using aggregation or disaggregation.
#'
#' @param x A [`SpatRaster`] to be resampled.
#' @param y A [`SpatRaster`] to which `x` will be resampled
#' @param discrete [`logical`] to specifiy if input raster has continitous or discrete values
#'
#' @returns [`SpatRaster`]
#'
#' @keywords spatial
#'
#' @seealso
#' \code{\link[terra]{aggregate}},
#' \code{\link[terra]{disagg}}
#'
#' @examples
#' set.seed(42)
#' ras_a <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
#' ymin = 0, ymax = 100, resolution = 20, crs = NA)
#'
#' ras_b <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
#' ymin = 0, ymax = 100, resolution = 5, crs = NA)
#'
#'  terra::values(ras_a) <- runif(n = terra::ncell(ras_a))
#'  terra::values(ras_b) <- runif(n = terra::ncell(ras_b))
#'
#'  sp_resampleRas(x = ras_a, y = ras_b)
#'
#' @export
sp_resampleRas <- function(x, y, discrete = FALSE) {

  # MH: Switch this to assertthat::assert_that()
  # check if CRS are already the same
  if (!terra::same.crs(x = x, y = y)) stop("No the same CRS!", call. = FALSE)

  # check how many layers are present
  if (terra::nlyr(x) > 1) stop("Only one layer allowed", call. = FALSE)

  # get factor levels
  if (discrete) cats_df <- terra::levels(x = x)

  # get resolution
  res_x <- terra::res(x)
  res_y <- terra::res(y)

  # get name
  name_x <- names(x)

  # current resolution finer than scale -> aggregate
  if (all(res_x < res_y)) {

    x <- suppressWarnings(
      terra::aggregate(x = x, fact = res_y / res_x, fun = ifelse(test = discrete,
                                                                 yes = "modal", no = "mean"))
    )

  # current resolution coarser than scale -> disaggregate
  } else if (all(res_x > res_y)) {

    x <- suppressWarnings(
      terra::disagg(x = x, fact = res_x / res_y, method = ifelse(test = discrete,
                                                                 yes = "near", no = "bilinear"))
    )

  } else {message("Nothing to do")}

  # re-sample predictor to background raster
  x <- terra::resample(x = x, y = y, method = ifelse(test = discrete, yes = "near", no = "bilinear"))

  # reset levels
  if (exists("cats_df")) levels(x) <- cats_df[[1]]

  # make sure name get preserved
  names(x) <- name_x

  return(x)

}
