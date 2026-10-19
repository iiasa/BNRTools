#' Unifies the extents of spatial objects
#'
#' Computes the combined (union) bounding extent of one or several spatial
#' objects. Supported inputs are \pkg{sf} / \code{sfc} objects, \pkg{stars}
#' objects, and \pkg{terra} \code{SpatRaster} (also \code{SpatVector} and
#' \code{SpatExtent}) objects. Extents are handled as \code{terra::SpatExtent}
#' objects internally.
#'
#' If the inputs have different coordinate reference systems (CRS), each
#' extent is projected to a common CRS before being combined, using
#' \code{terra::project()} on the extent (edges are densified, so the
#' projected extent fully contains the original one). The common CRS is the
#' one given in \code{crs}, or otherwise the CRS of the first input that has one.
#'
#' @param ... One or more \code{sf}, \code{sfc}, \code{stars}, \code{SpatRaster},
#'   \code{SpatVector} or \code{SpatExtent} objects. A single (plain) list of
#'   such objects is also accepted.
#' @param crs Optional target CRS in which the unified extent is returned.
#'   Can be an EPSG code (numeric), a string understood by \pkg{terra}
#'   (e.g. \code{"EPSG:3035"}, WKT, PROJ string) or an \pkg{sf} \code{crs}
#'   object. If \code{NULL} (default), the CRS of the first input with a
#'   defined CRS is used. Objects without a CRS are assumed to already be in
#'   the target CRS (a warning is issued).
#'
#' @return A named numeric vector \code{c(xmin, xmax, ymin, ymax)} giving the
#'   union of all input extents, in the units of the target CRS. Note that the
#'   order is \code{xmin, xmax, ymin, ymax} (the \pkg{terra} convention), which
#'   differs from \code{sf::st_bbox()}.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' r  <- rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:32633")
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Same CRS: simple union
#' r2 <- rast(xmin = 5, xmax = 20, ymin = -5, ymax = 8, crs = "EPSG:32633")
#' spl_unifyExtents(r, r2)
#'
#' # Mixed classes and CRSs, result in WGS84
#' spl_unifyExtents(nc, r, crs = "EPSG:4326")
#'
#' # Input as a list
#' spl_unifyExtents(list(r, r2))
#'
#' # Use the result as a SpatExtent
#' terra::ext(spl_unifyExtents(r, r2))
#' }
#'
#' @importFrom terra ext crs project same.crs
#' @importFrom sf st_bbox st_crs
#' @export
spl_unifyExtents <- function(..., crs = NULL) {

  objs <- list(...)

  # Allow a single plain list of objects: unify_extents(list(a, b, c))
  if (length(objs) == 1L && is.list(objs[[1L]]) && !is.object(objs[[1L]])) {
    objs <- objs[[1L]]
  }
  if (length(objs) == 0L) {
    stop("No spatial objects supplied.", call. = FALSE)
  }

  # Extract extent + CRS (as string) from every object
  info <- lapply(seq_along(objs), function(i) .extent_info(objs[[i]], i))

  # Drop objects with empty / NA extents
  valid <- vapply(info, function(x) !anyNA(as.vector(x$ext)), logical(1))
  if (!any(valid)) {
    stop("None of the supplied objects has a valid (non-empty) extent.",
         call. = FALSE)
  }
  if (!all(valid)) {
    warning("Ignoring ", sum(!valid), " object(s) with an empty extent.",
            call. = FALSE)
    info <- info[valid]
  }

  # Determine target CRS
  has_crs <- vapply(info, function(x) nzchar(x$crs), logical(1))
  if (!is.null(crs)) {
    target <- .crs_as_string(crs)
  } else if (any(has_crs)) {
    target <- info[[which(has_crs)[1L]]]$crs
  } else {
    target <- ""
  }

  if (nzchar(target) && any(!has_crs)) {
    warning("Some objects have no CRS; assuming they are already in the ",
            "target CRS.", call. = FALSE)
  }

  # Bring every extent to the target CRS and collect as xmin, xmax, ymin, ymax
  vals <- do.call(rbind, lapply(info, function(x) {
    e <- x$ext
    if (nzchar(target) && nzchar(x$crs) && !terra::same.crs(x$crs, target)) {
      e <- terra::project(e, from = x$crs, to = target)
    }
    unname(as.vector(e))
  }))

  c(xmin = min(vals[, 1L]),
    xmax = max(vals[, 2L]),
    ymin = min(vals[, 3L]),
    ymax = max(vals[, 4L]))
}


# ---- internal helpers --------------------------------------------------------

#' Extract a SpatExtent and CRS string from a supported object
#' @param x Spatial object.
#' @param i Position of the object in the input (for error messages).
#' @return A list with elements `ext` (SpatExtent) and `crs` (character,
#'   "" if undefined).
#' @noRd
.extent_info <- function(x, i = 1L) {

  if (inherits(x, "SpatExtent")) {
    list(ext = x, crs = "")

  } else if (inherits(x, c("SpatRaster", "SpatVector"))) {
    list(ext = terra::ext(x), crs = terra::crs(x))

  } else if (inherits(x, c("sf", "sfc", "stars"))) {
    bb <- sf::st_bbox(x)
    cr <- sf::st_crs(x)
    list(
      ext = terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]),
      crs = if (is.na(cr)) "" else cr$wkt
    )

  } else {
    stop("Object ", i, " (class: ", paste(class(x), collapse = "/"),
         ") is not supported. Use sf, stars, SpatRaster, SpatVector ",
         "or SpatExtent objects.", call. = FALSE)
  }
}

#' Coerce a user-supplied CRS to a string usable by terra
#' @noRd
.crs_as_string <- function(crs) {
  if (inherits(crs, "crs")) {
    crs$wkt
  } else if (is.numeric(crs)) {
    paste0("EPSG:", as.integer(crs))
  } else if (is.character(crs) && length(crs) == 1L) {
    crs
  } else {
    stop("`crs` must be NULL, an EPSG code, a CRS string or an sf 'crs' object.",
         call. = FALSE)
  }
}
