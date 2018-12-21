context("test-fg_connection")

test_that("throws is not FGConnection", {
  expect_error(assert_is_connection("bla"), "the connection is invalid, call fastgenomicsRClient::connect to obtain a valid connection")
})

test_that("must be FGConnection", {
  expect_silent(assert_is_connection(connect("http://example.com/", "Bearer ey1")))
})

