# RExport a gridded raster to a NetCDF format

This function serves as a general wrapper function to export a provided
spatial gridded layer as multi-dimensional NetCDF file. It furthermore
requires the specification of a list containing metadata information.

## Usage

``` r
spl_exportNetCDF(
  obj,
  filename,
  global_meta = system.file("iiasa_meta.yaml", package = "BNRTools"),
  separate_meta = FALSE,
  ...
)
```

## Arguments

- obj:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object to be exported.

- filename:

  A [`character`](https://rdrr.io/r/base/character.html) with the output
  filename.

- global_meta:

  A global metadata descriptor by default using IIASA standard metadata
  (Default: `"iiasa_meta data"`).

- separate_meta:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag on whether the
  metadata should be written separately in `"yaml"` format (Default:
  `FALSE`).

- ...:

  Any other metadata that should be overwritten or added to
  `"global_meta"`.

## Details

The default metadata is contained in `"inst/iiasa_meta"`. See examples.

## Note

A support for `'stars'` could be added.

## See also

[`writeCDF`](https://rspatial.github.io/terra/reference/writeCDF.html),
[`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Author

Martin Jung

## Examples

``` r
if (FALSE) { # \dontrun{
# Load default metadata (loaded by default by function too)
meta <- yaml::read_yaml(system.file("iiasa_meta.yaml", package = "BNRTools"))

# Dummy raster
obj <- terra::rast(ncol = 100, nrow = 100,
                   xmin = 0, xmax = 100,
                   ymin = 0, ymax = 100,
                   resolution = 5, crs = terra::crs("WGS84"),
                   val = runif(400)
                   )

# Export
spl_exportNetCDF(obj, filename = "test.nc",
                     global_meta = meta, title = "Super cool analysis")
} # }
```
