test_that("raster templates set output geometry and mask NA cells", {
  target <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:16
  )
  template <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = c(1, NA, 1, 1)
  )

  result <- spl_clipMask(target, template, method = "near")

  expect_true(terra::compareGeom(result, template))
  expect_true(is.na(terra::values(result)[2, 1]))
})

test_that("targets without values are rejected and geometry-only templates work", {
  empty <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326"
  )
  valid <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = 1:4
  )

  expect_error(spl_clipMask(empty, valid), "target must contain")
  result <- spl_clipMask(valid, empty)

  expect_true(terra::compareGeom(result, empty))
  expect_false(anyNA(terra::values(result)))
})

test_that("unsupported objects and undefined CRS are rejected", {
  target <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    crs = "EPSG:4326", vals = 1:4
  )
  no_crs <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)))
  )

  expect_error(spl_clipMask(data.frame(), target), "Supply a SpatRaster")
  expect_error(spl_clipMask(target, list()), "Supply a SpatRaster")
  expect_error(spl_clipMask(target, no_crs), "defined CRS")
})

test_that("raster templates reproject the target to their CRS", {
  target <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:16
  )
  template <- terra::project(target, "EPSG:3857", method = "near")

  result <- spl_clipMask(target, template, method = "near")

  expect_true(terra::compareGeom(result, template))
})

test_that("sf templates crop and rasterize a mask", {
  target <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:16
  )
  boundary <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(1, 1), c(3, 1), c(1, 3), c(1, 1)
    ))), crs = 4326)
  )

  result <- spl_clipMask(target, boundary, method = "near")

  expect_true(inherits(result, "SpatRaster"))
  expect_equal(terra::ncell(result), 4L)
  expect_true(anyNA(terra::values(result)))
  expect_true(any(!is.na(terra::values(result))))

  boundary_projected <- sf::st_transform(boundary, 3857)
  projected_result <- spl_clipMask(target, boundary_projected, method = "near")
  expect_true(terra::same.crs(projected_result, terra::vect(boundary_projected)))
})

test_that("vector file paths are read as sf templates", {
  target <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:16
  )
  boundary <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(1, 1), c(3, 1), c(1, 3), c(1, 1)
    ))), crs = 4326)
  )
  path <- tempfile(fileext = ".geojson")
  sf::st_write(boundary, path, driver = "GeoJSON", quiet = TRUE)
  on.exit(unlink(path), add = TRUE)

  result <- spl_clipMask(target, path, method = "near")

  expect_true(inherits(result, "SpatRaster"))
  expect_true(any(!is.na(terra::values(result))))
})

test_that("stars targets retain their class", {
  target <- terra::rast(
    nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:16
  )
  target_stars <- stars::st_as_stars(target)
  template <- terra::rast(
    nrows = 2, ncols = 2, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:4326", vals = 1:4
  )

  result <- spl_clipMask(target_stars, template, method = "near")

  expect_true(inherits(result, "stars"))
  expect_true(terra::compareGeom(terra::rast(result), template))
})