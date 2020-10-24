test_that("API version is numeric", {
  expect_identical(is.numeric(api_version()), TRUE)
  expect_equal(api_version(), 32)
})

test_that("API resource must be specified, points to analytics by default, and the logic works", {
  expect_error(api_resource(), "API resource must be specified!", ignore.case = T)
  expect_equal(api_resource("dataValueSets"), paste0("api/", api_version(), "/analytics/dataValueSets"))
  expect_equal(api_resource("metadata", push = T), paste0("api/", api_version(), "/metadata"))
})


