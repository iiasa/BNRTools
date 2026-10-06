# Sanitize variable names

Prepared covariates often have special characters in their variable
names which can or can not be used in formulas or cause errors for
certain procedures. This function converts special characters (points,
underscores or similar) of variable names into a species format.

## Usage

``` r
misc_sanitizeNames(names)
```

## Arguments

- names:

  A [`vector`](https://rdrr.io/r/base/vector.html) of
  [`character`](https://rdrr.io/r/base/character.html) vectors to be
  sanitized.

## Value

A [`vector`](https://rdrr.io/r/base/vector.html) of sanitized
[`character`](https://rdrr.io/r/base/character.html).

## Examples

``` r
# Correct variable names
vars <- c("Climate-temperature2015", "Elevation__sealevel", "Landuse.forest..meanshare")
misc_sanitizeNames(vars)
#> ℹ Note: Method deprecated. Just janitor::make_clean_names() directly!
#> [1] "climate_temperature2015"  "elevation_sealevel"      
#> [3] "landuse_forest_meanshare"
```
