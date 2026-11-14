test_that("small polygons merge into the same-class neighbour with the longest border", {
  polygons <- sf::st_sf(
    class = rep("forest", 3),
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 3), c(0, 3), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(4, 0), c(4, 1), c(2, 1), c(2, 0)))),
      sf::st_polygon(list(rbind(c(2, -2), c(4, -2), c(4, 0), c(2, 0), c(2, -2)))),
      crs = 3857
    )
  )

  result <- spl_mergeSmallPolygon(polygons, areaThreshold = 2.1, classColumn = "class")

  expect_equal(nrow(result), 2L)
  expect_equal(result$id, c(1L, 3L))
  expect_equal(as.numeric(sf::st_area(result[1, ])), 6)
  expect_equal(as.numeric(sf::st_area(result[2, ])), 6)
})

test_that("only smaller polygons with a shared border and matching non-missing class merge", {
  polygons <- sf::st_sf(
    class = c("forest", "water", "forest", NA_character_),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
      sf::st_polygon(list(rbind(c(4, 0), c(5, 0), c(5, 1), c(4, 1), c(4, 0)))),
      sf::st_polygon(list(rbind(c(5, 0), c(6, 0), c(6, 1), c(5, 1), c(5, 0)))),
      crs = 3857
    )
  )

  atThreshold <- spl_mergeSmallPolygon(polygons, areaThreshold = 1, classColumn = "class")
  expect_identical(atThreshold, polygons)

  result <- spl_mergeSmallPolygon(polygons, areaThreshold = 1.1, classColumn = "class")

  expect_equal(nrow(result), 4L)
  expect_identical(result, polygons)
})

test_that("arguments and polygon inputs are validated", {
  polygon <- sf::st_sf(
    class = "forest",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))), crs = 3857)
  )

  expect_error(spl_mergeSmallPolygon(data.frame(), 1, "class"), "sf object")
  expect_error(spl_mergeSmallPolygon(polygon, -1, "class"), "area threshold")
  expect_error(spl_mergeSmallPolygon(polygon, 1, "missing"), "class column")

  no_crs <- sf::st_sf(
    class = "forest",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))))
  )
  expect_error(spl_mergeSmallPolygon(no_crs, 2, "class"), "defined CRS")
})
