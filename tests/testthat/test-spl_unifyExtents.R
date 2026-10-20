test_that("sp_unifyExtents works as expected", {

  testthat::skip_if_not_installed('terra')
  testthat::skip_if_not_installed('sf')
  library(terra)
  library(sf)

  r  <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:32633")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"),quiet = TRUE)

  # Same CRS: simple union
  r2 <- terra::rast(xmin = 5, xmax = 20, ymin = -5, ymax = 8, crs = "EPSG:32633")

  # Tests
  expect_equal(
    spl_unifyExtents(r, r2),
    c(xmin = 0, xmax = 20, ymin = -5, ymax = 10)
  )

  # Input as a list
  expect_equal(
    spl_unifyExtents(list(r, r2)),
    c(xmin = 0, xmax = 20, ymin = -5, ymax = 10)
  )

  # Mixed classes and CRSs, result in WGS84
  expect_type(
    spl_unifyExtents(nc, r, crs = "EPSG:4326"),
    'double'
  )

  # Use the result as a SpatExtent
  expect_no_error(
    terra::ext(spl_unifyExtents(r, r2))
  )
})
