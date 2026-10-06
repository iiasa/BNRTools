# Package index

## Conversion functions

Key functions to convert model outputs for use in other modelling
environments.

- [`conv_downscalr2ibis()`](https://iiasa.github.io/BNRTools/reference/conv_downscalr2ibis.md)
  :

  Function to format a prepared GLOBIOM netCDF file for use in
  `ibis.iSDM`

## Spatial functions

Helper function combine, aggregate or otherwise modify spatial files.

- [`spl_resampleRas()`](https://iiasa.github.io/BNRTools/reference/spl_resampleRas.md)
  : Resample raster
- [`spl_exportNetCDF()`](https://iiasa.github.io/BNRTools/reference/spl_exportNetCDF.md)
  : RExport a gridded raster to a NetCDF format
- [`spl_replaceGriddedNA()`](https://iiasa.github.io/BNRTools/reference/spl_replaceGriddedNA.md)
  : Replace NA values in gridded layers with a fixed value.
- [`spl_growGrid()`](https://iiasa.github.io/BNRTools/reference/spl_growGrid.md)
  : Grow a categorical SpatRaster by certain amount of pixels or
  distance.
- [`spl_rwr()`](https://iiasa.github.io/BNRTools/reference/spl_rwr.md) :
  Calculates a rarity-weighted richness estimate from modelled species
  distributions
- [`spl_summarizeGPKGlayers()`](https://iiasa.github.io/BNRTools/reference/spl_summarizeGPKGlayers.md)
  : Summarize a set of layers from geopackages
- [`spl_clipMask()`](https://iiasa.github.io/BNRTools/reference/spl_clipMask.md)
  : Clip and mask a raster using a raster or vector template
- [`spl_mosaicTiffs()`](https://iiasa.github.io/BNRTools/reference/spl_mosaicTiffs.md)
  : Mosaic a list of spatial layers together.

## Miscellaneous functions

Any other functions that are generally useful for a wide range of
applications.

- [`` `%notin%` ``](https://iiasa.github.io/BNRTools/reference/grapes-notin-grapes.md)
  : Inverse of 'in' call
- [`misc_sanitizeNames()`](https://iiasa.github.io/BNRTools/reference/misc_sanitizeNames.md)
  : Sanitize variable names
- [`spl_replaceGriddedNA()`](https://iiasa.github.io/BNRTools/reference/spl_replaceGriddedNA.md)
  : Replace NA values in gridded layers with a fixed value.
- [`misc_objectSize()`](https://iiasa.github.io/BNRTools/reference/misc_objectSize.md)
  : Shows size of objects in the R environment
- [`misc_emptyfolder()`](https://iiasa.github.io/BNRTools/reference/misc_emptyfolder.md)
  : Check if a directory is empty (and create it if it doesn't exist)
