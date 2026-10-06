#' Summarize a set of layers from geopackages
#'
#' @description
#' Geopackages are a common format for storing spatial data, allowing
#' multiple layers within a single file. These can be for example point or polygon
#' data.
#' @param folder A [`character`] string specifying the path where the geopackage files are
#' stored. This looks specifically for files with the \code{'.gpkg'} extension, skipping others.
#' @param verbose A [`logical`] value indicating whether to print additional information during processing.
#' @return A [`data.frame`] summarizing the layers in the geopackage, including their names, geometry types, and feature counts.
#'
#' @examples
#' \dontrun{
#'  # Folder
#'  spl_summarizeGPKGlayers(folder = "path/to/your/geopackages")
#' }
#' @author Martin Jung
#' @importFrom sf st_layers
#' @importFrom tibble tibble
#' @export
spl_summarizeGPKGlayers <- function(folder, verbose = TRUE) {
  assertthat::assert_that(
    is.character(folder), dir.exists(dirname(folder)),
    msg = "The specified folder does not exist or is not a directory."
  )

  # Get all pkgs in the folder and look recursively
  ll <- list.files(folder, full.names = TRUE, recursive = TRUE)
  ll <- ll[assertthat::has_extension(ll, "gpkg")]

  # If no layers are found, return an empty data frame
  if (length(ll) == 0) {
    cli::cli_alert_warning("No geopackage files found in the specified folder.")
    return(data.frame())
  }

  # Output container
  out <- tibble::tibble()

  # Now summarize the layer contents for each
  if(verbose) pb <- progress::progress_bar$new(
    format = "  Summarizing layers [:bar] :percent eta: :eta",
    total = length(ll), clear = FALSE, width = 60
  )

  # Process
  for(ifname in ll){
    if(verbose) pb$tick()

    ff <- sf::st_layers(ifname, do_count = TRUE)

    # Format CRS name
    if(all(is.na(ff$crs))) {
      crs <- NA_character_
    } else {
      # Assuming all layers have the same crs
      crs <- ff$crs[[1]]$input
    }

    # Make summary out
    new <- tibble::tibble(
      folder = dirname(ifname),
      filename = tools::file_path_sans_ext(basename(ifname)),
      code = ff$name,
      geom = paste0(unique(unlist(ff$geomtype)), collapse = ";"),
      n = ff$features,
      crs = crs
    )

    # Append
    out <- dplyr::bind_rows(out, new)

    # Cleanup
    rm(crs, ff, new)
  }

  # return result
  return(out)
}
