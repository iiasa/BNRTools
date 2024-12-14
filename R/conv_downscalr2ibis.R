#' Function to format a prepared GLOBIOM netCDF file for use in \code{ibis.iSDM}
#'
#' @description
#' This function expects a downscaled GLOBIOM output as created in
#' the BIOCLIMA project. It converts the input to a stars object to be fed to
#' the \code{ibis.iSDM} R-package.
#'
#' @param fname A filename in [`character`] pointing to a GLOBIOM output in netCDF format.
#' @param ignore A [`vector`] of variables to be ignored (Default: \code{NULL}).
#' @param period A [`character`] limiting the period to be returned from the
#' formatted data. Options include \code{"reference"} for the first entry, \code{"projection"}
#' for all entries but the first, and \code{"all"} for all entries (Default: \code{"reference"}).
#' @param template An optional [`SpatRaster`] object towards which projects
#' should be transformed.
#' @param shares_to_area A [`logical`] on whether shares should be corrected to
#' areas (if identified).
#' @param use_gdalutils (Deprecated) [`logical`] on to use gdalutils hack-around.
#' @param verbose [`logical`] on whether to be chatty.
#'
#' @return A [`SpatRaster`] stack with the formatted GLOBIOM predictors.
#'
#' @keywords conversion
#'
#' @author Martin Jung
#' @examples
#' \dontrun{
#' ## Does not work unless downscalr file is provided.
#' # Expects a filename pointing to a netCDF file.
#' covariates <- conv_downscalr2ibis(fname)
#' }
#'
#' @export
conv_downscalr2ibis <- function(fname, ignore = NULL,
                          period = "all", template = NULL, shares_to_area = FALSE,
                          use_gdalutils = FALSE,
                          verbose = TRUE){
  assertthat::assert_that(
    file.exists(fname),
    assertthat::has_extension(fname, "nc"),
    is.null(ignore) || is.character(ignore),
    is.character(period),
    is.character(fname),
    is.logical(shares_to_area),
    is.logical(use_gdalutils),
    is.logical(verbose)
  )
  period <- match.arg(period, c("reference", "projection", "all"), several.ok = FALSE)

  # Try and load in the GLOBIOM file to get the attributes
  fatt <- ncdf4::nc_open(fname)
  if(verbose) cli::cli_alert_warning(paste0("[Setup] Found ", fatt$ndims, " dimensions and ", fatt$nvars, " variables"))

  # Get all dimension names and variable names
  dims <- names(fatt$dim)
  vars <- names(fatt$var)
  if(!is.null(ignore)) assertthat::assert_that( all( ignore %in% vars ) )

  attrs <- list() # For storing the attributes
  sc <- vector() # For storing the scenario files
  sc_area <- list() # For storing any area information if set

  # Now open the netcdf file with stars
  if( length( grep("netcdf", stars::detect.driver(fname), ignore.case = TRUE) )>0 ){
    if(verbose){
      cli::cli_alert_warning("[Predictor] Loading in predictor file...")
      pb <- progress::progress_bar$new(total = length(vars),
                                       format = "Loading :variable (:spin) [:bar] :percent")
    }

    for(v in vars) {
      if(verbose) pb$tick(tokens = list(variable = v))
      if(!is.null(ignore)) if(ignore == v) next()

      # Get and save the attributes of each variable
      attrs[[v]] <- ncdf4::ncatt_get(fatt, varid = v, verbose = FALSE)

      # Load in the variable
      suppressWarnings(
        suppressMessages(
          ff <- stars::read_ncdf(fname,
                                 var = v,
                                 proxy = FALSE,
                                 make_time = TRUE, # Make time on 'time' band
                                 make_units = FALSE # To avoid unnecessary errors due to unknown units
          )
        )
      )

      # Sometimes variables don't seem to have a time dimension
      if(!"time" %in% names(stars::st_dimensions(ff))) {
        if(shares_to_area && length(grep("area",names(ff)))>0){
          # Check that the unit is a unit
          if(fatt$var[[v]]$units %in% c("km2","ha","m2")){
            sc_area <- ff
          }
        } else {
          next()
        }
      }

      # Record dimensions for later
      full_dis <- stars::st_dimensions(ff)

      # Get dimensions other that x,y and time and split
      # Commonly used column names
      check = c("x","X","lon","longitude", "y", "Y", "lat", "latitude", "time", "Time", "year", "Year")
      chk <- which(!names(stars::st_dimensions(ff)) %in% check)

      if(length(chk)>0){
        for(i in chk){
          col_class <- names(stars::st_dimensions(ff))[i]
          # FIXME: Dirty hack to remove forest zoning
          if(length( grep("zone",col_class,ignore.case = T) )>0) next()

          # And class units as description from over
          class_units <- fatt$dim[[col_class]]$units
          class_units <-  class_units |>
            base::strsplit(";") |>
            # Remove emptyspace and special symbols
            sapply(function(y)  gsub("[^0-9A-Za-z///' ]", "" , y, ignore.case = TRUE) ) |>
            sapply(function(y)  gsub(" ", "" , y, ignore.case = TRUE) )
          # Convert to vector and make names
          class_units <- paste0(
            v, "__",
            make.names(unlist(class_units)) |> as.vector()
          )

          ff <- ff |> split(col_class) |> stats::setNames(nm = class_units)

          # FIXME: Dirty hack to deal with the forest zone dimension
          # If there are more dimensions than 3, aggregate over them
          if( length(stars::st_dimensions(ff)) >3){
            # Aggregate spatial-temporally
            ff <- stars::st_apply(ff, c("longitude", "latitude", "time"), sum, na.rm = TRUE)
          }
        }
      }

      # Finally aggregate
      if(!is.null(template) && inherits(template, "SpatRaster")){
        # FIXME: MJ 14/11/2022 - The code below is buggy, resulting in odd
        # curvilinear extrapolations for Europe Hacky approach now is to convert
        # to raster, crop, project and then convert back. Only use if gdalUtils
        # is installed
        # if(("gdalUtilities" %in% utils::installed.packages()[,1])&&use_gdalutils){
        #   ff <- ibis.iSDM:::hack_project_stars(ff, template, use_gdalutils)
        # } else {
          # Make background
          bg <- stars::st_as_stars(template)

          # # Get resolution
          res <- stars::st_res(bg)
          assertthat::assert_that(!anyNA(res))

          # # And warp by projecting and resampling
          ff <- ff |> stars::st_warp(bg, crs = sf::st_crs(bg),
                                     cellsize = res, method = "near") |>
            sf::st_transform(crs = sf::st_crs(template))
        # }
        # Overwrite full dimensions
        full_dis <- stars::st_dimensions(ff)
      }
      # Now append to vector
      sc <- c(sc, ff)
      rm(ff)
    }
    invisible(gc())
    assertthat::assert_that(length(names(full_dis))>=3)

    # Format sc object as stars and set dimensions again
    sc <- stars::st_as_stars(sc)
    assertthat::assert_that(length(sc)>0)
    full_dis <- full_dis[c(
      grep("x|longitude",names(full_dis), ignore.case = TRUE,value = TRUE),
      grep("y|latitude",names(full_dis), ignore.case = TRUE,value = TRUE),
      grep("year|time",names(full_dis), ignore.case = TRUE,value = TRUE)
    )] # Order assumed to be correct
    assertthat::assert_that(length(names(full_dis))==3)
    stars::st_dimensions(sc) <- full_dis # Target dimensions

  } else { cli::cli_abort("Fileformat not recognized!")}

  # Get time dimension (without applying offset) so at the centre
  times <- stars::st_get_dimension_values(sc, "time", center = TRUE)

  # Make checks on length of times and if equal to one, drop. check.
  if(length(times)==1){
    if(period == "projection") cli::cli_abort("Found only a single time slot. Projections not possible.")
    if(verbose) cli::cli_alert_warning('[Setup] Found only a single time point in file. Dropping time dimension.')
    # Drop the time dimension
    sc <- abind::adrop(sc, drop = which(names(stars::st_dimensions(sc)) == "time") )
  }

  # Formate times unit and convert to posix if not already set
  if(is.numeric(times) && length(times) > 1){
    # Assume year and paste0 as properly POSIX formatted
    times <- as.POSIXct( paste0(times, "-01-01") )
    sc <- stars::st_set_dimensions(sc, "time", times)
  }

  # Depending on the period, slice the input data
  if(period == "reference"){
    # Get the first entry and filter
    if(length(times)>1){
      # In case times got removed
      times_first <- stars::st_get_dimension_values(sc, "time")[1]
      sc <- sc |> dplyr::filter("time" == times_first)
      times <- times_first;rm(times_first)
    }
  } else if(period == "projection"){
    # Remove the first time entry instead, only using the last entries
    times_allbutfirst <- stars::st_get_dimension_values(sc, "time")[-1]
    sc <- sc |> dplyr::filter("time" %in% times_allbutfirst)
    times <- times_allbutfirst; rm(times_allbutfirst)
  }
  assertthat::assert_that(length(times)>0,
                          length(sc)>=1)

  # Create raster template if set
  if(!is.null(template)){
    # Check that template is a raster, otherwise rasterize for GLOBIOM use
    if(inherits(template, "sf")){
      o <- sc |> dplyr::slice("time" , 1) |> terra::rast()
      template <- terra::rasterize(template, o, field = 1)
      rm(o)
    }
  }

  # Correct shares to area if set
  if(shares_to_area && inherits(sc_area,"stars")){
    # Transform and warp the shares
    sc_area <- stars::st_warp(sc_area, stars::st_as_stars(template), crs = sf::st_crs(sc),method = "near")
    # grep those layers with the name share
    shares <- grep(pattern = "share|fraction|proportion", names(sc),value = TRUE)
    sc[shares] <- sc[shares] * sc_area
  }

  return( sc )
}

#' Deprecated formatting function
#' @description
#' This function is only kept for backwards compatability with old \code{ibis.iSDM}
#' code. Instead the new `conv_downscalr2ibis()` function should be used.
#' @param ... Parameters passed on [`conv_downscalr2ibis()`]
#' @inheritParams conv_downscalr2ibis
#' @returns None
#' @keywords spatial
#' @noRd
formatGLOBIOM <- function(...){
  cli::cli_alert_warning(c("formatGLOBIOM() is deprecated! ",
                "i" = "Use conv_downscalr2ibis() instead."))
  conv_downscalr2ibis(...)
}
