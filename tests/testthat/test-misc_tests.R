test_that("Basic unit tests for small misc functions", {
  expect_equal(
    suppressMessages(
      misc_sanitizeNames("Climate-temperature2015")
      ), "climate_temperature2015"
    )

  # Not in function\
  lu <- c("Forest", "Cropland", "Wetland", "OtherLand")
  lu2 <- c("Forest", "Wetland", "otherland")
  test <- which(lu %notin% lu2)
  expect_equal(test, c(2,4))
})
