# Replace NA values in gridded layers with a fixed value.

This function replaces all NA values in a spatial gridded layer with a
fixed value such as for example `0`. Accepted input layers are for
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
from the `"terra"` R-package and
[`stars`](https://rdrr.io/r/graphics/stars.html) from the `"stars"`
R-package.

## Usage

``` r
spl_replaceGriddedNA(obj, value = 0, mask, verbose = FALSE)
```

## Arguments

- obj:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  [`SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object.

- value:

  A fixed numeric value of which all `NA` values are to be replaced with
  (Default: `0`).

- mask:

  An optional
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object used instead of the value.

- verbose:

  Be chatty about what is processed (Default: `FALSE`).

## Value

A object of the same type as the input but with no-data values replaced
with `'value'`.

## Details

Required inputs are a single `"obj"` gridded data object and a numeric
value. In addition an optional mask layer can be provided that to use a
mask. In this case all no-data values a replaced with the value in this
mask.

## Author

Martin Jung

## Examples

``` r
# Example
s <- terra::rast(system.file("ex/logo.tif", package="terra"))
s[sample(1:terra::ncell(s), 100)] <- NA
sfill <- spl_replaceGriddedNA(s, value = 100)
terra::plot(sfill)

```
