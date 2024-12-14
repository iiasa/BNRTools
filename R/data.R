#' Table with default paths to commonly used spatial input files
#'
#' @description
#' This dataset contains path names to commonly-used spatial data files. Given that
#' those files are usually quite large, we here only describe where to find them internally
#' and not upload the data itself.
#' Medium-long-term this could be improved by relying on github LFS systems or our own gitlab
#' instance.
#'
#' @details
#' The file has the following columns:
#'
#' [*] 'drive': The path to drive where the data is stored (Default: \code{'P:/bnr/'}). Can be system dependent (Windows/Linux).
#' [*] 'access': A non-structured field containing the list of people that have access (for example \code{'IBF'} or \code{'bnr'}).
#' [*] 'group': A field entry describing to what this file belongs to (i.e. \code{"EPIC"}).
#' [*] 'filename': The actual filename
#'
#' @note
#' To update or overwrite, load the file and update, then apply
#' \code{usethis::use_data(bnr_datapaths, overwrite = TRUE) }.
#'
#' @keywords internal
#' @format A [data.frame] containing paths to key spatial data sources.
#' @source Manually updated and curated by BNR researchers
"bnr_datapaths"
