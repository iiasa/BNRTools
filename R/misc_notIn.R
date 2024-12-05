#' Inverse of 'in' call
#'
#' @description
#' Calculates the set of entries not present in the second vector.
#' Added for convenience since this is not supported by default in R.
#'
#' @param a First [`vector`] object.
#' @param b Second [`vector`] object.
#'
#' @returns A [`vector`] of [`logical`] entries of \code{'a'} not present in \code{'b'}.
#' @examples
#' # example code
# lu <- c("Forest", "Cropland", "Wetland", "OtherLand")
# lu2 <- c("Forest", "Wetland", "otherland")
# which(lu %notin% lu2)
#' @keywords utils
#' @author Martin Jung
#' @export
`%notin%` = function(a, b){!(a %in% b)}
