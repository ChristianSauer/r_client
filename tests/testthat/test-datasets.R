context("test-datasets")

BEARER_FROM_ENV = Sys.getenv("BEARERTOKEN")
BASE_URL = Sys.getenv("BASEURL")

test_that("scope is valid", {
  default_conn <- new("FGConnection", base_url = "invalid", bearer_token = "invalid")
  expect_error(get_datasets(default_conn, "bla"), "scope is 'bla' but should be one of: All, Public, Private")
})


test_that("can get successfully", {
  browser()
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  datasets <- get_datasets(default_conn, "All")@content
  n_all_datasets = length(datasets)
  expect_gte(n_all_datasets, 1 )

  datasets <- get_datasets(default_conn, "Private")@content
  n_private_datasets = length(datasets)
  expect_lt(n_private_datasets, n_all_datasets )
})
