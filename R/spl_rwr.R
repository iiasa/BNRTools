#' Calculates a rarity-weighted richness estimate from modelled species distributions
#'
#' @description
#' This function calculates a rarity-weighted richness estimate from modelled species distributions,
#' which can for example be obtained from the [`ibis.iSDM`] R-package. The input maps should ideally be binary
#' presence-absence maps, but the function can also handle continuous predictions.
#'
#' @details
#' The function
#'
#' @param x A [`SpatRaster`] or alternatively [`data.frame`] object containing the modelled species distributions.
#' @param normalize A [`logical`] flag on whether to normalize the rarity-weighted richness ranks.
#' @param column An optional [`character`] value on whether the (Default: \code{NULL}).
#'
#' @return A logical value: `TRUE` if the directory is empty (or newly created), `FALSE` otherwise.
#' @author Martin Jung
#' @examples
#' \dontrun{
#' # Calculate rarity-weighted richness from a [`SpatRaster`].
#'
#' }
#' @references
#' * Albuquerque, F., & Beier, P. (2016). Predicted rarity‐weighted richness, a new tool to prioritize sites for species representation. Ecology and Evolution, 6(22), 8107-8114.
#' * Albuquerque, F., & Beier, P. (2015). Rarity-weighted richness: a simple and reliable alternative to integer programming and heuristic algorithms for minimum set and maximum coverage problems in conservation planning. PloS one, 10(3), e0119905.
#'
spl_rwr <- function(x, normalize = TRUE, column = NULL) {
  # Check if x is a SpatRaster
  if (inherits(x, "SpatRaster")) {
    # Convert SpatRaster to data.frame
    x <- as.data.frame(x, xy = TRUE)
  }

  # Check if x is a data.frame
  if (!is.data.frame(x)) {
    stop("Input must be a SpatRaster or data.frame.")
  }

  # Calculate rarity-weighted richness
  rwr <- rowSums(x[, -c(1, 2)], na.rm = TRUE)

  # Normalize if required
  if (normalize) {
    rwr <- rwr / max(rwr, na.rm = TRUE)
  }

  return(rwr)
}
