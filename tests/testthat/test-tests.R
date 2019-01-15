context("test-tests")

test_that("connection needs Bearer Token", {
  expect_error(fastgenomicsRclient::connect("http://example.com/", ""), "The Bearer Token should look like 'Bearer ey.....'")
})

test_that("connection bearer token need to appear valid", {
  expect_error(fastgenomicsRclient::connect("http://example.com/", "B"), "The Bearer Token should look like 'Bearer ey.....'")
  expect_error(fastgenomicsRclient::connect("http://example.com/", "Bearer"), "The Bearer Token should look like 'Bearer ey.....'")
  expect_silent(fastgenomicsRclient::connect("http://example.com/", "Bearer ey1"))
})


test_that("base_url is valid", {
  expect_silent(fastgenomicsRclient::connect("http://example.com/", "Bearer ey1"))
})

test_that("expect s4 return", {
  expect_s4_class(fastgenomicsRclient::connect("http://example.com/", "Bearer ey1"), "FGConnection")
})

test_that("expect s4 return bearer token", {
  url <- "http://example.com/"
  token <- "Bearer ey1"
  expect_equal (fastgenomicsRclient::connect(url, token)@bearer_token, token)
})

test_that("expect s4 return url", {
  url <- "http://example.com/"
  expect_equal (fastgenomicsRclient::connect(url, "Bearer ey1")@base_url, url)
})
