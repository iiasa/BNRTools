#' Sanitize variable names
#'
#' @description Prepared covariates often have special characters in their
#' variable names which can or can not be used in formulas or cause errors for
#' certain procedures. This function converts special characters (points, underscores or similar)
#' of variable names into a species format.
#'
#' @param names A [`vector`] of [`character`] vectors to be sanitized.
#'
#' @returns A [`vector`] of sanitized [`character`].
#'
#' @keywords utils
#' @concept Inspired from [`inlabru`] \code{"bru_standardise_names"} function.
#'
#' @examples
#' # Correct variable names
#' vars <- c("Climate-temperature2015", "Elevation__sealevel", "Landuse.forest..meanshare")
#' misc_sanitizeNames(vars)
#'
#' @export
misc_sanitizeNames <- function(names){
  assertthat::assert_that(
    length(names) > 0
  )
  cli::cli_alert_info("Note: Method deprecated. Just janitor::make_clean_names() directly!")
  return(
    janitor::make_clean_names(names)
  )

  # Convert the variable names
  new_names <- vapply(names, function(x) {
    gsub("[-() ]", "_", x = x, fixed = FALSE)
  }, "name")

  not_ok <- grepl("__", x = new_names)
  while (any(not_ok)) {
    new_names[not_ok] <- vapply(new_names[not_ok], function(x) {
      gsub("__", "_", x = x, fixed = FALSE)
    }, "name")
    not_ok <- grepl("__", x = new_names)
  }
  assertthat::assert_that(length(new_names) == length(names))
  return(
    as.character(new_names)
  )
}
