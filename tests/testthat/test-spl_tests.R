test_that("Spatial object modifications", {

  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  # Dummy layers
  r1 <- terra::rast(nrows = 10, ncols = 10, res = 0.05, xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, vals = rnorm(3600,mean = .5,sd = .1))
  r2 <- terra::rast(nrows = 10, ncols = 10, res = 0.05, xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, vals = rnorm(3600,mean = .5,sd = .5))
  expect_s4_class(r1, "SpatRaster")
  expect_s4_class(r2, "SpatRaster")

  # --- #
  # NA replacements #
  r1[sample(1:terra::ncell(r1),100)] <- NA
  expect_no_error(
    r1_filled <- spl_replaceGriddedNA(r1, value = 0)
  )
  expect_s4_class(r1_filled, "SpatRaster")
  expect_equal(terra::global(r1_filled,"min")[,1], 0) # Should be 0
  # Use layer 2 for masking instead
  expect_no_error(
    r1_filled2 <- spl_replaceGriddedNA(r1,mask = r2)
  )
  expect_s4_class(r1_filled2, "SpatRaster")
  # --- #
})
