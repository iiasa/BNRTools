# Calculates a rarity-weighted richness estimate from modelled species distributions

This function calculates a rarity-weighted richness estimate from
modelled species distributions, which can for example be obtained from
the `` `ibis.iSDM` `` R-package. The input maps should ideally be binary
presence-absence maps, but the function can also handle continuous
predictions.

## Usage

``` r
spl_rwr(x, normalize = TRUE, column = NULL)
```

## Arguments

- x:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or alternatively
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) object
  containing the modelled species distributions.

- normalize:

  A [`logical`](https://rdrr.io/r/base/logical.html) flag on whether to
  normalize the rarity-weighted richness ranks.

- column:

  An optional [`character`](https://rdrr.io/r/base/character.html) value
  on whether the (Default: `NULL`).

## Value

A logical value: `TRUE` if the directory is empty (or newly created),
`FALSE` otherwise.

## Details

The function

## References

- Albuquerque, F., & Beier, P. (2016). Predicted rarity‐weighted
  richness, a new tool to prioritize sites for species representation.
  Ecology and Evolution, 6(22), 8107-8114.

- Albuquerque, F., & Beier, P. (2015). Rarity-weighted richness: a
  simple and reliable alternative to integer programming and heuristic
  algorithms for minimum set and maximum coverage problems in
  conservation planning. PloS one, 10(3), e0119905.

## Author

Martin Jung

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate rarity-weighted richness from a [`SpatRaster`].

} # }
```
