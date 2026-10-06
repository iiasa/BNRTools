# Summarize a set of layers from geopackages

Geopackages are a common format for storing spatial data, allowing
multiple layers within a single file. These can be for example point or
polygon data.

## Usage

``` r
spl_summarizeGPKGlayers(folder, verbose = TRUE)
```

## Arguments

- folder:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the path where the geopackage files are stored. This looks
  specifically for files with the `'.gpkg'` extension, skipping others.

- verbose:

  A [`logical`](https://rdrr.io/r/base/logical.html) value indicating
  whether to print additional information during processing.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) summarizing the
layers in the geopackage, including their names, geometry types, and
feature counts.

## Author

Martin Jung

## Examples

``` r
if (FALSE) { # \dontrun{
 # Folder
 spl_summarizeGPKGlayers(folder = "path/to/your/geopackages")
} # }
```
