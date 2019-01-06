context("test-workflows")
context("test-apps")

BEARER_FROM_ENV = Sys.getenv("BEARERTOKEN")
BASE_URL = Sys.getenv("BASEURL")

test_that("can get workflows successfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  n_public_workflows = length(public_workflows)
  expect_gte(n_public_workflows, 1 )

  public_workflows <- get_workflows(default_conn, "Private")@content
  n_private_workflows = length(public_workflows)
  expect_lte(n_private_workflows, n_public_workflows )
})

test_that("can get a workflow sucessfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_workflow(default_conn, id)
  workflow_id <- app@content[["id"]]
  expect_equal(workflow_id, id)
})

test_that("can get a workflow with fullDetail sucessfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]
  workflow <- get_workflow(default_conn, id, fullDetail = TRUE)
  expect_gt(length(workflow@content[["calculation_flow"]]), 0)
})

test_that("can get an edit model sucessfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_edit_model_of_workflow(default_conn, id)
  workflow_title <- app@content[["title"]]
  expect_equal(workflow_title, public_workflows[[1]][["title"]])
})

test_that("create a workflow works", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_edit_model_of_workflow(default_conn, id)
  tmp <- tempfile()
  readr::write_file(httr::content(app@response, "text"), tmp)

  new_workflow <- create_workflow(default_conn, tmp)
  expect_match(new_workflow@content[["id"]], "wf_*")
})

test_that("create a workflow rejects empty file", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)

  expect_error(create_workflow(default_conn, "tmp"), "The file")
})

test_that("create a workflow rejects files not looking like json", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  tmp <- tempfile()
  readr::write_file("{", tmp)
  expect_error(create_workflow(default_conn, tmp), "The file")
})

test_that("can save a workflow to disk", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  tmp <- tempfile()
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_workflow(default_conn, id)

  save_workflow_as_file(app, tmp)

  str <- readr::read_file(tmp)

  expect_true(jsonlite::validate(str))
})

test_that("modify a workflow works", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_edit_model_of_workflow(default_conn, id)
  tmp <- tempfile()
  readr::write_file(httr::content(app@response, "text"), tmp)

  new_workflow <- create_workflow(default_conn, tmp)
  id <- new_workflow@content[["id"]]
  updated_workflow <- modify_workflow(default_conn, tmp, id)
  expect_match(updated_workflow@Id, id)
  version <- updated_workflow@content[["version"]]
  expect_gte(as.integer(version), 2)
})
