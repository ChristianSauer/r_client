context("test-tests")

test_that("connection needs Bearer Token", {
  expect_error(fastgenomicsRclient::connect(""), "The Bearer Token cannot be empty!")
})

test_that("connection bearer token need to appear valid", {
  expect_error(fastgenomicsRclient::connect("B"), "The Bearer Token should look like 'Bearer ey.....'")
  expect_error(fastgenomicsRclient::connect("Bearer"), "The Bearer Token should look like 'Bearer ey.....'")
  expect_silent(fastgenomicsRclient::connect("Bearer ey1"))
})

