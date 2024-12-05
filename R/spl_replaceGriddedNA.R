#' Replace NA values in gridded layers with a fixed value.
#'
#' @description
#' This function replaces all NA values in a spatial gridded layer with
#' a fixed value such as for example \code{0}. Accepted input layers are for [`SpatRaster`] from
#' the \code{"terra"} R-package and [`stars`] from the \code{"stars"} R-package.
#'
#' @details
#' Required inputs are a single \code{"obj"} gridded data object and a numeric value.
#' In addition an optional mask layer can be provided that to use a mask. In this case
#' all no-data values a replaced with the value in this mask.
#'
#' @param obj A [`SpatRaster`], [`SpatRasterDataset`] or [`stars`] object.
#' @param value A fixed numeric value of which all \code{NA} values are to be replaced with (Default: \code{0}).
#' @param mask An optional [`SpatRaster`] object used instead of the value.
#' @param verbose Be chatty about what is processed (Default: \code{FALSE}).
#'
#' @examples
#' # Example
#' s <- terra::rast(system.file("ex/logo.tif", package="terra"))
#' s[sample(1:terra::ncell(s), 100)] <- NA
#' sfill <- sp_replaceGriddedNA(s, value = 100)
#' terra::plot(sfill)
#'
#' @returns A object of the same type as the input but with no-data values replaced with \code{'value'}.
#' @author Martin Jung
#' @keywords internal, utils
#' @export
sp_replaceGriddedNA <- function(obj, value = 0, mask, verbose = FALSE){
  assertthat::assert_that(
    inherits(obj, "stars") || inherits(obj, "SpatRaster"),
    is.numeric(value) || length(value)==1,
    missing(mask) || (inherits(mask, "stars") || inherits(mask, "SpatRaster")),
    is.logical(verbose),
    msg = "Input data or parameters wrong set?"
  )

  # Process
  if(missing(mask)){
    if(verbose) message("Replacing no data values with value: ", value)
    # Set all NA to the value
    obj[is.na(obj)] <- value
  } else {
    if(verbose) message("Masking obj with mask values.")
    # Mask the layer by template
    obj[is.na(obj)] <- mask[is.na(obj)]
  }
  return(obj)
}
