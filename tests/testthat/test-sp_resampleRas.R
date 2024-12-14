ras_a <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
ymin = 0, ymax = 100, resolution = 20, crs = NA)

ras_b <- terra::rast(ncol = 100, nrow = 100, xmin = 0, xmax = 100,
ymin = 0, ymax = 100, resolution = 5, crs = NA)

terra::values(ras_a) <- runif(n = terra::ncell(ras_a))
terra::values(ras_b) <- runif(n = terra::ncell(ras_b))

test_that("sp_resampleRas results in same resolution/extent", {

  ras_a_res <- spl_resampleRas(x = ras_a, y = ras_b)

  expect_true(terra::compareGeom(ras_a_res, ras_b))

})
