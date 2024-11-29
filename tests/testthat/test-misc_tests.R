test_that("Basic unit tests for misc functions", {
  expect_equal(misc_sanitizeNames("Climate-temperature2015"), "Climate_temperature2015")
})
