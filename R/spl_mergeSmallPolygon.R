#' Merge small polygons with adjacent polygons of the same class
#'
#' @description
#' Assigns each polygon smaller than an area threshold to the adjacent polygon
#' of the same class with which it shares the longest border, then unions the
#' assigned geometries. Small polygons without a larger, same-class polygon
#' sharing a border remain unchanged. Equal border lengths are resolved in
#' favour of the larger polygon.
#'
#' @param x An `sf` object containing only polygon or multipolygon features and
#'   a defined coordinate reference system.
#' @param areaThreshold Numeric area cutoff. Polygons with area strictly below
#'   this value are considered small. Use the units returned by
#'   `sf::st_area(x)` (square metres for a geographic CRS).
#' @param classColumn Name of the attribute column identifying polygon class.
#'   Features with missing class values are not merged.
#'
#' @returns An `sf` object with assigned small features removed and their
#'   geometries unioned into their recipient features. Recipient attributes are
#'   retained; unassigned features are unchanged.
#'
#' @author Martin Jung
#' @keywords spatial
#' @seealso [sf::st_area()], [sf::st_touches()], [sf::st_union()]
#'
#' @examples
#' polygons <- sf::st_sf(
#'   class = c("forest", "forest", "water"),
#'   id = 1:3,
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
#'     sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
#'     sf::st_polygon(list(rbind(c(3, 0), c(4, 0), c(4, 1), c(3, 1), c(3, 0)))),
#'     crs = 3857
#'   )
#' )
#' merged <- spl_mergeSmallPolygon(polygons, areaThreshold = 1.1, classColumn = "class")
#' nrow(merged)
#' @export
spl_mergeSmallPolygon <- function(x, areaThreshold, classColumn) {
  # Check inputs
  assertthat::assert_that(inherits(x, "sf"), msg = "`x` must be an sf object.")
  assertthat::assert_that(
    nrow(x) > 0L,
    is.numeric(areaThreshold) && length(areaThreshold) == 1L &&
      !is.na(areaThreshold) && is.finite(areaThreshold) && areaThreshold >= 0,
    is.character(classColumn) && length(classColumn) == 1L &&
      !is.na(classColumn) && nzchar(classColumn) && classColumn %in% names(x),
    msg = "Supply a non-empty sf object, a finite non-negative area threshold, and an existing class column."
  )

  # Extract class values and geometry types
  classValues <- x[[classColumn]]
  geometryTypes <- as.character(sf::st_geometry_type(x))
  assertthat::assert_that(
    is.atomic(classValues) && is.null(dim(classValues)),
    all(geometryTypes %in% c("POLYGON", "MULTIPOLYGON")),
    !any(sf::st_is_empty(x)),
    !is.na(sf::st_crs(x)),
    msg = "`x` must have a class column and non-empty polygon geometries with a defined CRS."
  )

  # Identify small polygons
  areas <- as.numeric(sf::st_area(x))
  assertthat::assert_that(all(is.finite(areas)), msg = "Polygon areas must be finite.")
  small <- areas < areaThreshold
  if (!any(small)) return(x)

  ## Sparse adjacency and local boundary intersections avoid an all-pairs overlay.
  neighbours <- sf::st_touches(x, sparse = TRUE)
  geometries <- sf::st_geometry(x)
  smallIndexes <- which(small)
  # Identify candidate recipient polygons for each small polygon
  candidateSets <- lapply(smallIndexes, function(i) {
    candidates <- neighbours[[i]]
    if (!length(candidates) || is.na(classValues[i])) return(integer())
    sameClass <- !is.na(classValues[candidates]) & classValues[candidates] == classValues[i]
    candidates[!small[candidates] & sameClass]
  })
  hasCandidates <- lengths(candidateSets) > 0L
  if (!any(hasCandidates)) return(x)

  # Filter the small polygons and their candidates
  smallIndexes <- smallIndexes[hasCandidates]
  candidateSets <- candidateSets[hasCandidates]
  boundaryIndexes <- sort(unique(c(smallIndexes, unlist(candidateSets, use.names = FALSE))))
  boundaries <- sf::st_boundary(geometries[boundaryIndexes])
  boundaryIndex <- integer(nrow(x))
  boundaryIndex[boundaryIndexes] <- seq_along(boundaryIndexes)
  recipient <- rep(NA_integer_, nrow(x))

  # Compute shared boundary lengths and assign recipients
  pb <- cli::cli_progress_bar(
    format = "Assigning small polygons [:bar] :percent eta: :eta",
    total = length(smallIndexes), clear = FALSE, width = 60
  )
  for (k in seq_along(smallIndexes)) {
    cli::cli_progress_update(pb)
    i <- smallIndexes[k]
    candidates <- candidateSets[[k]]
    sharedLength <- vapply(candidates, function(j) {
      as.numeric(sum(sf::st_length(sf::st_intersection(
        boundaries[boundaryIndex[i]], boundaries[boundaryIndex[j]]
      ))))
    }, numeric(1))
    candidates <- candidates[sharedLength > 0]
    sharedLength <- sharedLength[sharedLength > 0]
    if (!length(candidates)) next

    best <- candidates[sharedLength == max(sharedLength)]
    recipient[i] <- best[which.max(areas[best])]
  }
  cli::cli_progress_done(pb)
  rm(pb)

  # Merge the small polygons with their recipients
  assigned <- which(!is.na(recipient))
  if (!length(assigned)) return(x)

  groups <- split(assigned, recipient[assigned])
  for (targetName in names(groups)) {
    target <- as.integer(targetName)
    geometries[target] <- sf::st_union(geometries[c(target, groups[[targetName]])])
  }
  sf::st_geometry(x) <- geometries
  return(
    x[-assigned, , drop = FALSE]
  )

}
