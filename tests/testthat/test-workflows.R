context("test-workflows")
context("test-apps")

BEARER_PAT = Sys.getenv("FG_PAT")
BEARER_EMAIL = Sys.getenv("FG_EMAIL")
BASE_URL = Sys.getenv("FGBASEURL")
default_conn <- FGConnection$new(base_url = BASE_URL, pat = BEARER_PAT, email = BEARER_EMAIL)

test_that("can get workflows successfully", {
  public_workflows <- get_workflows(default_conn, "All")@content
  n_public_workflows = length(public_workflows)
  expect_gte(n_public_workflows, 1 )

  public_workflows <- get_workflows(default_conn, "Private")@content
  n_private_workflows = length(public_workflows)
  expect_lte(n_private_workflows, n_public_workflows )
})

test_that("can get a workflow sucessfully", {
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_workflow(default_conn, id)
  workflow_id <- app@content[["id"]]
  expect_equal(workflow_id, id)
})

test_that("can get a workflow with fullDetail sucessfully", {
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]
  workflow <- get_workflow(default_conn, id, fullDetail = TRUE)
  expect_gt(length(workflow@content[["calculation_flow"]]), 0)
})

test_that("can get an edit model sucessfully", {
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_edit_model_of_workflow(default_conn, id)
  workflow_title <- app@content[["title"]]
  expect_equal(workflow_title, public_workflows[[1]][["title"]])
})

test_that("create a workflow works", {
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_edit_model_of_workflow(default_conn, id)
  tmp <- tempfile()
  readr::write_file(httr::content(app@response, "text"), tmp)

  new_workflow <- create_workflow(default_conn, tmp)
  expect_match(new_workflow@content[["id"]], "wf_*")
})

test_that("create a workflow rejects empty file", {
  expect_error(create_workflow(default_conn, "tmp"), "The file")
})

test_that("create a workflow rejects files not looking like json", {
  tmp <- tempfile()
  readr::write_file("{", tmp)
  expect_error(create_workflow(default_conn, tmp), "The file")
})

test_that("can save a workflow to disk", {
  tmp <- tempfile()
  public_workflows <- get_workflows(default_conn, "All")@content
  id = public_workflows[[1]][["id"]]

  app <- get_workflow(default_conn, id)

  save_workflow_as_file(app, tmp)

  str <- readr::read_file(tmp)

  expect_true(jsonlite::validate(str))
})

test_that("modify a workflow works", {
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
