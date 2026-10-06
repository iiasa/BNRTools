#' Clip and mask a raster using a raster or vector template
#'
#' @description
#' Aligns a [`SpatRaster`] or [`stars`] raster to a raster template, or clips it
#' to a vector template. Vector geometries are rasterized on the target grid
#' and used as a mask.
#'
#' @param x A [`SpatRaster`] or [`stars`] object to clip and mask.
#' @param template A [`SpatRaster`] or [`stars`] raster template, an [`sf`] or
#'   `sfc` object, or a path to a raster or vector file.
#' @param method Resampling method: `"bilinear"` for continuous data or `"near"`
#'   for categorical data (Default: `"bilinear"`).
#' @param touches If `TRUE`, rasterize all cells touched by vector geometries;
#'   otherwise rasterize cells whose centers fall within them (Default: `FALSE`).
#'
#' @returns An object of the same class as `x`, with its layer count and names
#'   preserved. Raster templates set the output grid and, when they contain
#'   values, mask cells where the first layer is `NA`. Vector templates crop
#'   the output to their extent and mask it to the rasterized geometry.
#'
#' @author Martin Jung
#' @keywords spatial
#' @seealso [terra::crop()], [terra::extend()], [terra::mask()],
#'   [terra::project()], [terra::rasterize()], [terra::resample()]
#'
#' @examples
#' target <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10,
#'                       ymin = 0, ymax = 10, crs = "EPSG:4326", vals = 1:100)
#' boundary <- sf::st_sf(
#'   id = 1,
#'   geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#'     c(2, 2), c(8, 2), c(8, 8), c(2, 8), c(2, 2)
#'   ))), crs = 4326)
#' )
#' clipped <- spl_clipMask(target, boundary)
#'
#' @export
spl_clipMask <- function(x, template, method = c("bilinear", "near"), touches = FALSE) {
  message("Clipping and masking raster to template.")

  valid_template <- inherits(template, c("SpatRaster", "stars", "sf", "sfc")) ||
    (is.character(template) && length(template) == 1L && !is.na(template) && file.exists(template))
  assertthat::assert_that(
    inherits(x, "SpatRaster") || inherits(x, "stars"),
    valid_template,
    is.character(method) && length(method) > 0L && !anyNA(method),
    is.logical(touches) && length(touches) == 1L && !is.na(touches),
    msg = "Supply a SpatRaster or stars target and a raster, sf, or vector-file template."
  )
  method <- match.arg(method)

  if (is.character(template)) {
    # Try raster loading first; non-raster paths are read as vectors.
    raster_template <- tryCatch(terra::rast(template), error = function(e) NULL)
    template <- if (is.null(raster_template)) {
      sf::st_read(template, quiet = TRUE)
    } else {
      raster_template
    }
  }

  # Use terra for processing, then restore the input raster class.
  was_stars <- inherits(x, "stars")
  target <- if (was_stars) terra::rast(x) else x
  input_layers <- terra::nlyr(target)
  input_names <- names(target)
  assertthat::assert_that(
    input_layers > 0L,
    terra::hasValues(target),
    msg = "The target must contain at least one layer with values."
  )

  vector_template <- inherits(template, c("sf", "sfc"))
  if (vector_template) {
    feature_count <- if (inherits(template, "sf")) nrow(template) else length(template)
    assertthat::assert_that(
      feature_count > 0L,
      msg = "The vector template must contain at least one feature."
    )
    template <- terra::vect(template)
  } else {
    if (inherits(template, "stars")) template <- terra::rast(template)
    assertthat::assert_that(
      terra::nlyr(template) > 0L,
      msg = "The raster template must contain at least one layer."
    )
  }

  target_crs <- terra::crs(target)
  template_crs <- terra::crs(template)
  assertthat::assert_that(
    !is.na(target_crs) && nzchar(target_crs),
    !is.na(template_crs) && nzchar(template_crs),
    msg = "The target and template must both have a defined CRS."
  )

  if (vector_template) {
    # Rasterize the vector on the target grid to create its mask.
    if (!terra::same.crs(target, template)) {
      target <- terra::project(target, template_crs, method = method)
    }

    target_extent <- terra::ext(target)
    template_extent <- terra::ext(template)
    overlaps <- terra::xmin(target_extent) < terra::xmax(template_extent) &&
      terra::xmax(target_extent) > terra::xmin(template_extent) &&
      terra::ymin(target_extent) < terra::ymax(template_extent) &&
      terra::ymax(target_extent) > terra::ymin(template_extent)
    assertthat::assert_that(overlaps, msg = "The target and vector template do not overlap.")

    target <- terra::extend(target, template_extent, snap = "out")
    target <- terra::crop(target, template_extent, snap = "out")
    rasterized_template <- terra::rasterize(
      template, target, field = 1, background = NA, touches = touches
    )
    target <- terra::mask(target, rasterized_template)
  } else {
    if (!terra::same.crs(target, template)) {
      target <- terra::project(target, template, method = method)
    } else {
      target_extent <- terra::ext(target)
      template_extent <- terra::ext(template)
      overlaps <- terra::xmin(target_extent) < terra::xmax(template_extent) &&
        terra::xmax(target_extent) > terra::xmin(template_extent) &&
        terra::ymin(target_extent) < terra::ymax(template_extent) &&
        terra::ymax(target_extent) > terra::ymin(template_extent)
      if (overlaps) {
        target <- terra::extend(target, template_extent, snap = "out")
        target <- terra::crop(target, template_extent, snap = "out")
      }
      if (!terra::compareGeom(target, template, stopOnError = FALSE)) {
        target <- terra::resample(target, template, method = method)
      }
    }
    # Populated raster templates also provide the mask; empty ones define the grid only.
    if (terra::hasValues(template)) target <- terra::mask(target, template[[1]])
  }

  names(target) <- input_names
  result <- if (was_stars) stars::st_as_stars(target) else target
  result_raster <- if (was_stars) terra::rast(result) else result
  # Check that conversion preserved layers and aligned the output geometry.
  assertthat::assert_that(
    inherits(result, if (was_stars) "stars" else "SpatRaster"),
    terra::nlyr(result_raster) == input_layers,
    identical(names(result_raster), input_names),
    msg = "Output validation failed: raster class, layer count, or names changed."
  )

  if (vector_template) {
    assertthat::assert_that(
      terra::same.crs(result_raster, template),
      msg = "Output validation failed: raster and vector CRS differ."
    )
  } else {
    assertthat::assert_that(
      terra::compareGeom(result_raster, template, stopOnError = FALSE),
      msg = "Output validation failed: raster geometry does not match the template."
    )
  }

  result
}