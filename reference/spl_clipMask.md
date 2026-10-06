# Clip and mask a raster using a raster or vector template

Aligns a
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
or [`stars`](https://rdrr.io/r/graphics/stars.html) raster to a raster
template, or clips it to a vector template. Vector geometries are
rasterized on the target grid and used as a mask.

## Usage

``` r
spl_clipMask(x, template, method = c("bilinear", "near"), touches = FALSE)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) object to clip and
  mask.

- template:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or [`stars`](https://rdrr.io/r/graphics/stars.html) raster template,
  an [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or `sfc`
  object, or a path to a raster or vector file.

- method:

  Resampling method: `"bilinear"` for continuous data or `"near"` for
  categorical data (Default: `"bilinear"`).

- touches:

  If `TRUE`, rasterize all cells touched by vector geometries; otherwise
  rasterize cells whose centers fall within them (Default: `FALSE`).

## Value

An object of the same class as `x`, with its layer count and names
preserved. Raster templates set the output grid and, when they contain
values, mask cells where the first layer is `NA`. Vector templates crop
the output to their extent and mask it to the rasterized geometry.

## See also

[`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html),
[`terra::extend()`](https://rspatial.github.io/terra/reference/extend.html),
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html),
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html),
[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html),
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html)

## Author

Martin Jung

## Examples

``` r
target <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10,
                      ymin = 0, ymax = 10, crs = "EPSG:4326", vals = 1:100)
boundary <- sf::st_sf(
  id = 1,
  geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(2, 2), c(8, 2), c(8, 8), c(2, 8), c(2, 2)
  ))), crs = 4326)
)
clipped <- spl_clipMask(target, boundary)
#> Clipping and masking raster to template.
```
