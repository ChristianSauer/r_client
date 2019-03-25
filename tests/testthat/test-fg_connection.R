context("test-fg_connection")

test_that("Error when base_url is empty", {
    expect_error(FGConnection$new("", "PAT", "EMAIL"))
})

test_that("Error when PAT is empty", {
  expect_error(FGConnection$new("URL", "", "EMAIL"))
})

test_that("Error when EMAIL is empty", {
  expect_error(FGConnection$new("URL", "PAT", ""))
})
