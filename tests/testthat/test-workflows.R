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
