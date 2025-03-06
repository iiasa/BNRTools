#' RExport a gridded raster to a NetCDF format
#'
#' @description
#' This function serves as a general wrapper function to export a provided
#' spatial gridded layer as multi-dimensional NetCDF file. It furthermore requires
#' the specification of a list containing metadata information.
#'
#' @details
#' The default metadata is contained in \code{"inst/iiasa_meta"}. See examples.
#'
#' @note
#' A support for \code{'stars'} could be added.
#'
#' @param obj A [`SpatRaster`] object to be exported.
#' @param filename A [`character`] with the output filename.
#' @param global_meta A global metadata descriptor by default using IIASA standard
#' metadata (Default: \code{"iiasa_meta data"}).
#' @param separate_meta A [`logical`] flag on whether the metadata should be
#' written separately in \code{"yaml"} format (Default: \code{FALSE}).
#' @param ... Any other metadata that should be overwritten or added to \code{"global_meta"}.
#'
#' @returns NULL
#'
#' @author Martin Jung
#'
#' @keywords spatial
#'
#' @seealso
#' \code{\link[terra]{writeCDF}},
#' \code{\link[terra]{writeRaster}}
#'
#' @examples
#' \dontrun{
#' # Load default metadata (loaded by default by function too)
#' meta <- yaml::read_yaml(system.file("iiasa_meta.yaml", package = "BNRTools"))
#'
#' # Dummy raster
#' obj <- terra::rast(ncol = 100, nrow = 100,
#'                    xmin = 0, xmax = 100,
#'                    ymin = 0, ymax = 100,
#'                    resolution = 5, crs = terra::crs("WGS84"),
#'                    val = runif(400)
#'                    )
#'
#' # Export
#' spl_exportNetCDF(obj, filename = "test.nc",
#'                      global_meta = meta, title = "Super cool analysis")
#' }
#' @export
spl_exportNetCDF <- function(obj,
                             filename,
                             global_meta = system.file("iiasa_meta.yaml", package = "BNRTools"),
                             separate_meta = FALSE,
                             ...){
  # Checks
  assertthat::assert_that(
    inherits(obj, "SpatRaster") || inherits(obj, "stars"),
    is.character(filename),
    is.character(global_meta) || is.list(global_meta),
    is.logical(separate_meta)
  )
  # Check that output dir is writeable
  assertthat::assert_that(
    assertthat::is.writeable(dirname(filename)),
    msg = "Note that output directory is not writeable..."
  )
  # Check that output name has correct extension
  if(!tools::file_ext(filename) %in% c("nc","NC")){
    filename <- paste0(filename, ".nc")
  }

  # --- #
  # Load global metadata
  if(is.character(global_meta)){
    atrs <- yaml::read_yaml(global_meta)
  } else { atrs <- global_meta}
  # Check level of attribute and flatten otherwise
  if(purrr::pluck_depth(atrs) > 2) atrs <- purrr::flatten(atrs)

  # Check that list is correct
  assertthat::assert_that(
    is.list(atrs),
    length(atrs)>1,
    purrr::pluck_depth(atrs)>=2
  )
  # Load other fields and add them to the list or replace
  mc <- list(...)
  if(length(mc)>0){
    if(any(names(mc) %in% names(atrs))){
      ind <- names(mc)[which( names(mc) %in% names(atrs) )]
      for(k in ind){
        atrs[[k]] <- mc[[k]]
        mc[[k]] <- NULL # Remove
      }
    }
  }

  # Check common fields and raise warnings if empty
  if(utils::hasName(atrs,"title") & atrs[['title']] == ""){
    cli::cli_alert_warning("No set title found (empty attribute).")
  }

  # --- #
  # Create nc file
  nc <- RNetCDF::create.nc(filename = filename)

  # Get range of coordinates and define them
  focal_x <- terra::crds(obj)[,1] |> unique()
  focal_y <- terra::crds(obj)[,2] |> unique()

  # Define x dimension
  RNetCDF::dim.def.nc( nc, dimname = "lon", dimlength = length(focal_x))
  # Define x variable
  RNetCDF::var.def.nc(nc, varname = "lon", vartype = "NC_DOUBLE", dimensions = "lon")
  # Add attributes
  RNetCDF::att.put.nc(nc, variable = "lon", name = "units",
                      type = "NC_CHAR", value = atrs[['geospatial_lon_units']])
  RNetCDF::att.put.nc(nc, variable = "lon", name = "standard_name",
                      type = "NC_CHAR", value = atrs[['geospatial_lon_name']])
  RNetCDF::att.put.nc(nc, variable = "lon", name = "long_name",
                      type = "NC_CHAR", value = atrs[['geospatial_lon_name']])
  # Put data
  RNetCDF::var.put.nc(nc, variable = "lon", data = focal_x)

  # Define y dimension
  RNetCDF::dim.def.nc(nc, dimname = "lat", dimlength = length(focal_y))
  # Define y variable
  RNetCDF::var.def.nc(nc, varname = "lat", vartype = "NC_DOUBLE", dimensions = "lat")
  # Add attributes
  RNetCDF::att.put.nc(nc, variable = "lat", name = "units",
                      type = "NC_CHAR", value = atrs[['geospatial_lat_units']])
  RNetCDF::att.put.nc(nc, variable = "lat", name = "standard_name",
                      type = "NC_CHAR", value = atrs[['geospatial_lat_name']])
  RNetCDF::att.put.nc(nc, variable = "lat", name = "long_name",
                      type = "NC_CHAR", value = atrs[['geospatial_lat_name']])
  # Put data
  RNetCDF::var.put.nc(nc, variable = "lat", data = focal_y)

  # --- #
  # Define non-dimensional geographic projectioncrs variable
  RNetCDF::var.def.nc(nc, varname = "crs", vartype = "NC_CHAR", dimensions = NA)

  # Add attributes
  RNetCDF::att.put.nc(nc, variable = "crs", name = "long_name", type = "NC_CHAR", value = "Coordinate Reference System")
  RNetCDF::att.put.nc(nc, variable = "crs", name = "geographic_crs_name",
                      type = "NC_CHAR", value = ifelse(terra::is.lonlat(terra::crs(obj)), "WGS 84", "Custom"))
  RNetCDF::att.put.nc(nc, variable = "crs", name = "reference_ellipsoid_name",
                      type = "NC_CHAR", value = ifelse(terra::is.lonlat(terra::crs(obj)), "WGS 84", "Custom"))
  RNetCDF::att.put.nc(nc, variable = "crs", name = "grid_mapping_name", type = "NC_CHAR",
                      value = ifelse(terra::is.lonlat(terra::crs(obj)), "latitude_longitude", "Custom"))
  RNetCDF::att.put.nc(nc, variable = "crs", name = "horizontal_datum_name",
                      type = "NC_CHAR", value = ifelse(terra::is.lonlat(terra::crs(obj)), "WGS 84", "Custom"))
  RNetCDF::att.put.nc(nc, variable = "crs", name = "prime_meridian_name", type = "NC_CHAR", value = "Greenwich")
  RNetCDF::att.put.nc(nc, variable = "crs", name = "longitude_of_prime_meridian", type = "NC_DOUBLE", value = 0.)
  RNetCDF::att.put.nc(nc, variable = "crs", name = "semi_major_axis", type = "NC_DOUBLE", value = 6378137.)
  RNetCDF::att.put.nc(nc, variable = "crs", name = "semi_minor_axis", type = "NC_DOUBLE", value = 6356752.314245179)
  RNetCDF::att.put.nc(nc, variable = "crs", name = "inverse_flattening", type = "NC_DOUBLE", value = 298.257223563)
  RNetCDF::att.put.nc(nc, variable = "crs",name = "spatial_ref", type = "NC_CHAR", value = terra::crs(obj))
  # RNetCDF::att.put.nc(nc, variable = "crs", name = "GeoTransform", type = "NC_CHAR",
  #            value = '-180 0.08333333333333333 0 90 0 -0.08333333333333333 ')

  # Get variable names
  vnames <- names(obj)

  # Create and fill variables
  for(i in vnames){
    # Create the diversity metric variable defined by the four dimensions
    RNetCDF::var.def.nc(nc, varname = i, vartype = "NC_DOUBLE", dimensions = c("lon", "lat"))
    # Add no data fill values
    RNetCDF::att.put.nc(nc, variable = i, name = "_FillValue", type = "NC_DOUBLE", value = -99999)

    vnames_val <- paste(basename(filename), i)
    RNetCDF::att.put.nc(nc, variable = i,
                        name = "long_name", type = "NC_CHAR", value = vnames_val)

    # add data
    focal_array <- base::array(
      terra::values(obj)[,1],
      dim = c(length(focal_y), length(focal_x))
    )
    RNetCDF::var.put.nc(nc, variable = i, data = t(focal_array))
  }

  # --- #
  # Internal function to add the global attributes
  add_global_attributes <- function(nc, attributes){
    assertthat::assert_that(
      is.list(attributes)
    )

    # Loop through attributes
    for(i in 1:length(attributes)){
      if(is.character(attributes[[i]])){
        type <- "NC_CHAR"
      }else if(is.numeric(attributes[[i]])){
        type <- "NC_DOUBLE"
      }
      RNetCDF::att.put.nc(nc, variable = "NC_GLOBAL", name = names(attributes[i]),
                          type = type, value = attributes[[i]])
    }
    RNetCDF::sync.nc(nc)
  }

  # Add attributes
  add_global_attributes(nc, attributes = atrs)

  RNetCDF::sync.nc(nc)
  RNetCDF::close.nc(nc)

  # Check whether metadata should also be written
  if(separate_meta){
    ofname <- paste0(tools::file_path_sans_ext(filename), ".yaml")
    yaml::write_yaml(x = atrs,file = ofname)
    cli::cli_alert_info("Wrote separate metadata to file.")
  }

  # Final checks
  assertthat::assert_that(
    file.exists(filename)
  )
  cli::cli_alert_success("Succesfully written output format!")
}
