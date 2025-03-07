#' Mosaic a list of spatial layers together.
#'
#' @description
#' Particular for large spatial files, it is often impractical
#' to process the entire layer as single file. Instead such a file
#' could be split in smaller chunks and processed as such.
#'
#' A common issue then is to reconstruct a spatial file of the original extent.
#' This function takes a list of filenames as input and mosaics them
#' together using a \code{'vrt'} file format.
#'
#' @details
#' This function by default uses the \code{'gdalUtilities'} R-package and tools
#' for most of the projections.
#'
#' @param files A [`character`] vector with filenames of spatial files.
#' @param ofname A [`character`] where the output should be written (Default: \code{NULL}).
#' @param tempdir A [`character`] with a temporary folder that must exist (Default: \code{NULL}).
#' @param dt A [`character`] with the output datatype of the spatial file (Default: \code{"INT2S"}).
#' @param ... Any other parameters passed to [`writeRaster`].
#'
#' @returns A [`SpatRaster`] or just a file.
#'
#' @keywords spatial
#' @author Martin Jung
#' @seealso
#' \code{\link[terra]{mosaic}},
#' \code{\link[gdalUtilities]{gdalbuildvrt}}
#'
#' @examples
#' \dontrun{
#'  # Get list of files
#'  ll <- list.files(path_to_folder)
#'
#'  # Mosaic
#'  spl_mosaicTiffs(ll, "full_file.tif")
#' }
#'
#' @export
spl_mosaicTiffs <- function(files,
                            ofname = NULL,
                            tempdir = NULL,
                            dt = "INT2S",
                            ...
                            ) {
  # Checks
  assertthat::assert_that(
    all( file.exists(files) ),
    is.null(ofname) || is.character(ofname),
    is.null(tempdir) || dir.exists(tempdir),
    is.character(dt)
  )

  # Match output data.type
  dt <- match.arg(dt, c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S",
                        "FLT4S", "FLT8S"), several.ok = FALSE)

  # Check if the output has correct extension,
  # Otherwise add
  if(!is.null(ofname)){
    assertthat::assert_that(length(ofname)==1,
                            msg = "Supply a single output filename. ")
    ex <- tools::file_ext(ofname)
    if(ex %notin% c('tif', 'TIF')){
      cli::cli_alert_warning('Did not detect tif as extension. Appending...')
      ofname <- paste0(ofname, '.tif')
    }
  }

  # Set temporary folder if set
  if(!is.null(tempdir)){
    terra::terraOptions(tempdir = tempdir)
  } else {
    tempdir <- base::tempdir()
  }

  # Check files for right extension
  ex <- tools::file_ext(files)
  if(any(ex!='tif')){
    cli::cli_alert_warning("Found non-spatial files (non tif extension). Excluding...")
    files <- files[which(ex %in% c('tif','TIF'))]
  }

  # --- #
  cli::cli_alert_info(paste("Now starting to composite", length(files), "files."))

  vrt_file <- paste0(tempdir, 'input_tiff_list', '.vrt')
  run <- suppressWarnings(
    gdalUtilities::gdalbuildvrt(gdalfile = files,
                                output.vrt = vrt_file,
                                resolution = 'highest',
                                separate = FALSE
                                     )
  )
  assertthat::assert_that(length(run)>0,
                          msg = "Vrt building failed somehow?")
  # Check that all files are there
  assertthat::assert_that(
    all(terra::vrt_tiles(vrt_file) %in% files),
    msg = "Vrt file does not contain all inputs? Aborting..."
  )

  # Now export
  new <- terra::rast(vrt_file)
  if(!is.null(ofname)){
    cli::cli_alert_info(paste("Now writing output."))
    if(file.exists(ofname)) cli::cli_alert_danger("Overwriting existing file!")
    # Correcting filename
    names(new) <- tools::file_path_sans_ext(ofname)

    # Save output
    terra::writeRaster(x = new,
                       filename = ofname,
                       gdal = c("COMPRESS=DEFLATE"),
                       tempdir = tempdir,
                       datatype = dt,
                       ...
    )
    if(file.exists(ofname)){
      cli::cli_alert_success("Successfull wrote output file!")
    } else {
      cli::cli_alert_danger("Something went wrong with the output creation?")
    }
  } else {
    return( new )
  }
}
