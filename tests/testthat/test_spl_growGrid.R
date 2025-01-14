test_that("Testing to grow a grid", {

  suppressWarnings(requireNamespace("terra", quietly = TRUE))

  set.seed(42)
  ras <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
  ymin = 0, ymax = 100, resolution = 10, crs = NA)

  # Fill with dummy values but keep half as NA
  terra::values(ras) <- c(rep(NA,50),rbinom(terra::ncell(ras)/2, 10, 0.5))

  # Convert to factor
  ras <- terra::as.factor(ras)
  # Check
  testthat::expect_s4_class(ras, "SpatRaster")
  testthat::expect_true(terra::is.factor(ras))

  # Grow grid
  testthat::expect_no_error(
    ras_nona <- spl_growGrid(x = ras, iter = 10)
  )
  testthat::expect_s4_class(ras_nona, "SpatRaster")
  # Expect no NA to remain
  testthat::expect_false( terra::global(ras_nona, "anyNA")[,1] )
})
