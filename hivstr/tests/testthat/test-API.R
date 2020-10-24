test_that("API version is numeric", {
  expect_identical(is.numeric(api_version()), TRUE)
  expect_equal(api_version(), 32)
})

test_that("API resource must be specified, points to analytics by default, and the logic works", {
  expect_error(api_resource(), "API resource must be specified!", ignore.case = T)
  expect_equal(api_resource("dataValueSets"), paste0("api/", api_version(), "/analytics/dataValueSets"))
  expect_equal(api_resource("metadata", push = T), paste0("api/", api_version(), "/metadata"))
})


test_that("API endpoint defautls to hotspots", {
  expect_equal(api_endpoint(), paste0("https://clone.psi-mis.org/", api_resource("dataValueSet"),"?dimension=dx:dQTWxMDtAiW;mOGarPwHuFc&dimension=n5ODfcdD1YQ:YF8v2OSxWKl&dimension=ou:rP1W74RpNWF&dimension=pe:THIS_MONTH&completedOnly=FALSE"))
})

test_that("API response is present", {
  expect_error(api_status_check(), "API response must be specified")
})
