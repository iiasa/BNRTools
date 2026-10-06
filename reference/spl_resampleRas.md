# Resample raster

Resample two raster to the spatial resolution using aggregation or
disaggregation.

## Usage

``` r
spl_resampleRas(x, y, discrete = FALSE)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  to be resampled.

- y:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  to which `x` will be resampled

- discrete:

  [`logical`](https://rdrr.io/r/base/logical.html) to specifiy if input
  raster has continitous or discrete values

## Value

[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)

## See also

[`aggregate`](https://rspatial.github.io/terra/reference/aggregate.html),
[`disagg`](https://rspatial.github.io/terra/reference/disaggregate.html)

## Examples

``` r
set.seed(42)
ras_a <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
ymin = 0, ymax = 100, resolution = 20, crs = NA)

ras_b <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
ymin = 0, ymax = 100, resolution = 5, crs = NA)

 terra::values(ras_a) <- runif(n = terra::ncell(ras_a))
 terra::values(ras_b) <- runif(n = terra::ncell(ras_b))

 spl_resampleRas(x = ras_a, y = ras_b)
#> class       : SpatRaster
#> size        : 20, 20, 1  (nrow, ncol, nlyr)
#> resolution  : 5, 5  (x, y)
#> extent      : 0, 100, 0, 100  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> source(s)   : memory
#> name        :    lyr.1
#> min value   : 0.082438
#> max value   : 0.983614
```
