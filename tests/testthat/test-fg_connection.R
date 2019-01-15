context("test-fg_connection")

test_that("throws is not FGConnection", {
  expect_error(assert_is_connection("bla"), "the connection is invalid, call fastgenomicsRClient::connect to obtain a valid connection")
})

test_that("must be FGConnection", {
  expect_silent(assert_is_connection(connect("http://example.com/", "Bearer ey1")))
})

test_that("Error when base_url is empty", {
    expect_error(result =  new("FGConnection", base_url = "", bearer_token = "Bearer ey1"))
})

test_that("Invalid bearer_token", {
    expect_error(result =  new("FGConnection", base_url = "http://example.com", bearer_token = "abc"))
})
