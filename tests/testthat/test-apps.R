context("test-apps")

BEARER_PAT <- Sys.getenv("FG_PAT")
BEARER_EMAIL <- Sys.getenv("FG_EMAIL")
BASE_URL <- Sys.getenv("FGBASEURL")
default_conn <- FGConnection$new(BASE_URL, BEARER_PAT, BEARER_EMAIL)

test_that("can get apps successfully", {
  public_apps <- get_apps(default_conn, "All")@content
  n_public_apps = length(public_apps)
  expect_gte(n_public_apps, 1 )

  public_apps <- get_apps(default_conn, "Private")@content
  n_private_apps = length(public_apps)
  expect_lte(n_private_apps, n_public_apps )
})

test_that("can get an app sucessfully", {
  public_apps <- get_apps(default_conn, "All")@content
  image_name = public_apps[[1]][["image_name"]]

  app <- get_app(default_conn, image_name)
  app_id <- app@content[["image_name"]]
  expect_equal(app_id, image_name)
})

test_that("can create an app successfully", {
  random_name <- stringi::stri_rand_strings(n = 1, length = 20)
  result <- create_app(default_conn, "library/busybox:latest", registry = "registry.hub.docker.com" , image_name = stringr::str_interp("r_client_test_${random_name}:1") )
  expect_is(result, "FGResponse")
})

test_that("can create errors", {
  result <- create_app(default_conn, "library/busybox", registry = "registry.hub.docker.com")
  expect_is(result, "FGValidationProblemResponse")
})

test_that("poll: works", {
  random_name <- stringi::stri_rand_strings(n = 1, length = 20)
  result <- create_app(default_conn, "library/busybox:latest", registry = "registry.hub.docker.com" , image_name = stringr::str_interp("r_client_test_${random_name}:1"), username = "a", password = "b" )

  expect_false(poll_app_until_validated(default_conn, result, 1 ))
})
