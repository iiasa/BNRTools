# Mosaic a list of spatial layers together.

Particular for large spatial files, it is often impractical to process
the entire layer as single file. Instead such a file could be split in
smaller chunks and processed as such.

A common issue then is to reconstruct a spatial file of the original
extent. This function takes a list of filenames as input and mosaics
them together using a `'vrt'` file format.

## Usage

``` r
spl_mosaicTiffs(files, ofname = NULL, tempdir = NULL, dt = "INT2S", ...)
```

## Arguments

- files:

  A [`character`](https://rdrr.io/r/base/character.html) vector with
  filenames of spatial files.

- ofname:

  A [`character`](https://rdrr.io/r/base/character.html) where the
  output should be written (Default: `NULL`).

- tempdir:

  A [`character`](https://rdrr.io/r/base/character.html) with a
  temporary folder that must exist (Default: `NULL`).

- dt:

  A [`character`](https://rdrr.io/r/base/character.html) with the output
  datatype of the spatial file (Default: `"INT2S"`).

- ...:

  Any other parameters passed to
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or just a file.

## Details

This function by default uses the `'gdalUtilities'` R-package and tools
for most of the projections.

## See also

[`mosaic`](https://rspatial.github.io/terra/reference/mosaic.html),
[`gdalbuildvrt`](https://rdrr.io/pkg/gdalUtilities/man/gdalbuildvrt.html)

## Author

Martin Jung

## Examples

``` r
if (FALSE) { # \dontrun{
 # Get list of files
 ll <- list.files(path_to_folder)

 # Mosaic
 spl_mosaicTiffs(ll, "full_file.tif")
} # }
```
