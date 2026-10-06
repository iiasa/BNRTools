# Function to format a prepared GLOBIOM netCDF file for use in `ibis.iSDM`

This function expects a downscaled GLOBIOM output as created in the
BIOCLIMA project. It converts the input to a stars object to be fed to
the `ibis.iSDM` R-package.

## Usage

``` r
conv_downscalr2ibis(
  fname,
  ignore = NULL,
  period = "all",
  template = NULL,
  shares_to_area = FALSE,
  use_gdalutils = FALSE,
  verbose = TRUE
)
```

## Arguments

- fname:

  A filename in [`character`](https://rdrr.io/r/base/character.html)
  pointing to a GLOBIOM output in netCDF format.

- ignore:

  A [`vector`](https://rdrr.io/r/base/vector.html) of variables to be
  ignored (Default: `NULL`).

- period:

  A [`character`](https://rdrr.io/r/base/character.html) limiting the
  period to be returned from the formatted data. Options include
  `"reference"` for the first entry, `"projection"` for all entries but
  the first, and `"all"` for all entries (Default: `"reference"`).

- template:

  An optional
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object towards which projects should be transformed.

- shares_to_area:

  A [`logical`](https://rdrr.io/r/base/logical.html) on whether shares
  should be corrected to areas (if identified).

- use_gdalutils:

  (Deprecated) [`logical`](https://rdrr.io/r/base/logical.html) on to
  use gdalutils hack-around.

- verbose:

  [`logical`](https://rdrr.io/r/base/logical.html) on whether to be
  chatty.

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
stack with the formatted GLOBIOM predictors.

## Author

Martin Jung

## Examples

``` r
if (FALSE) { # \dontrun{
## Does not work unless downscalr file is provided.
# Expects a filename pointing to a netCDF file.
covariates <- conv_downscalr2ibis(fname)
} # }
```
