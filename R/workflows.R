library(readr)

#' Get all workflows
#'
#' This list does only contain valid, usable datasets
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param scope Filters the workflows by their scope. Possible Values are: 'All': return all workflows, 'Private': Only your personal workflows, 'Public': Only public workflows
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' workflows <- fastgenomicsRclient::get_workflows(connection)
#' print(workflows@content) # all workflows available to you
get_workflows <- function(connection, scope="All"){
  url <-  paste(connection$base_url, "workflow/api/v1/workflows", sep = "")

  result <- get_data_list(connection, scope, url, "workflow")

  return(result)
}

#' Get a specific workflow
#'
#' This model is not suitable for editing OR as base template for new workflows, call get_edit_model_of_workflow to obtain such a workflow!
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param workflow_id The id of the workflow, usually looks like wf_.....
#' @param fullDetail If TRUE, the calculationflow, screenflow and parameters are included.
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' workflows <- fastgenomicsRclient::get_workflow(connection, "wf_abc")
#' print(workflows@content) # the workflow
get_workflow <-  function(connection, workflow_id, fullDetail=FALSE){
  url <-  paste(connection$base_url, "workflow/api/v1/workflows/", workflow_id , sep = "")

  result <- get_data(connection, workflow_id, url, "workflow", queries = list("fullDetail" = fullDetail))

  return(result)
}

#' Get a specific workflow as an editable model
#'
#' This model is suitable to be used as a base for new workflows.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param workflow_id The id of the workflow, usually looks like wf_.....
#'
#' @return A FGResponse object if no errors, otherwisea a FGErrorResponse
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' workflow <- fastgenomicsRclient::get_edit_model_of_workflow(connection, "wf_abc")
#' print(workflows@content) # the workflows as an easily usable model
get_edit_model_of_workflow <- function(connection, workflow_id){
  url <-  paste(connection$base_url, "workflow/api/v1/workflows/", workflow_id , sep = "")

  result <- get_data(connection, workflow_id, url, "workflow", additional_headers = httr::accept("application/vnd.fastgenomics.editworkflow+json") )

  return(result)
}

#' Creates a new workflow on the server.
#'
#' This method expects a file with valid json as input, since creating a workflow is best done by hand. Use a good editor like VS Code for this task.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param path_to_workflow The file path to the edit model JSON.
#'
#' @return A FGResponse object if no errors, otherwisea a FGErrorResponse
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#'  # get a workflow as example
#' workflow <- fastgenomicsRclient::get_edit_model_of_workflow(connection, "wf_abc")
#' print(workflows@content) # the workflows as an easily usable model
#' path <- "workflow.json"
#' save_workflow_as_file(workflow, path) # save the workflow on disk
#' # make edits on the file...
#' result <- create_workflow(connection, path) # create the workflow
#' # check that the Result is not an FGErrorResponse, if it is an error check the "errors" field on the object.
create_workflow <- function(connection, path_to_workflow){
  assert_is_connection(connection)

  if (!file.exists(path_to_workflow)) {
    stop(stringr::str_interp("The file ${path_to_workflow} does not exist. Please provide a valid file."))
  }

  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "workflow/api/v1/workflows", sep = "")
  body = readr::read_file(path_to_workflow)

  is_valid_json = jsonlite::validate(body)
  if (!is_valid_json) {
    stop(stringr::str_interp("The file ${path_to_workflow} does not appear to be json! Please provide a valid json file. Hint: Check the json structure using a text editor like VS Code."))
  }
  headers <- headers <- c(headers, httr::content_type_json())
  response <- httr::POST(url, headers, body = body)
  if (response["status_code"] == 422) {
    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    error <- new("FGErrorResponse", path = url, content = parsed, validation_errors = parsed[["errors"]])
    return(error)
  }

  httr::stop_for_status(response) # abort on all other errors

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
  workflow_id <- parsed[["id"]]
  result <- new("FGResponse", path = url, content = parsed, DataType="workflow", Id = workflow_id, response = response )
  return(result)
}

#' Modifies an existing workflow
#'
#' Your modifications will not affect any existing analyses or analysistemplates. A successfull modification will increase the version field of the workflow.
#' You can update an analysistemplate through AnalysisApi: /api/v1/analysistemplates/{id}/update_workflow
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param path_to_workflow The file path to the edit model JSON.
#' @param workflow_id The id of the workflow to be updated
#'
#' @return A FGResponse object if no errors, otherwisea a FGErrorResponse
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#'  # get a workflow as example
#' workflow <- fastgenomicsRclient::get_edit_model_of_workflow(connection, "wf_abc")
#' print(workflows@content) # the workflows as an easily usable model
#' path <- "workflow.json"
#' save_workflow_as_file(workflow, path) # save the workflow on disk
#' # make edits on the file...
#' result <- create_workflow(connection, path) # create the workflow
#' updated_workflow <- modify_workflow(connection, path, result@content[["id"]]) # update the workflow
#' print(updated_workflow@content[["version"]]) # should be 2.0
modify_workflow <- function(connection, path_to_workflow, workflow_id){
  assert_is_connection(connection)

  if (!file.exists(path_to_workflow)) {
    stop(stringr::str_interp("The file ${path_to_workflow} does not exist. Please provide a valid file."))
  }

  if (!startsWith(workflow_id, "wf_")) {
    stop(stringr::str_interp("The workflow_id '${workflow_id}' must start with wf_"))
  }

  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "workflow/api/v1/workflows/", workflow_id, sep = "")
  body = readr::read_file(path_to_workflow)

  is_valid_json = jsonlite::validate(body)
  if (!is_valid_json) {
    stop(stringr::str_interp("The file ${path_to_workflow} does not appear to be json! Please provide a valid json file. Hint: Check the json structure using a text editor like VS Code."))
  }
  headers <- headers <- c(headers, httr::content_type_json())
  response <- httr::PUT(url, headers, body = body)
  if (response["status_code"] == 422) {
    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    error <- new("FGErrorResponse", path = url, content = parsed, validation_errors = parsed[["errors"]])
    return(error)
  }

  httr::stop_for_status(response) # abort on all other errors

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
  workflow_id <- parsed[["id"]]
  result <- new("FGResponse", path = url, content = parsed, DataType = "workflow", Id = workflow_id, response = response )
  return(result)
}

#' Saves the JSON of a workflow as a file
#'
#' Works on edit_models and normal workflows
#'
#' @param workflow A FGResponse object with DataType == "workflow"
#' @param path The path to write the json to
#'
#' @return NULL
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#'  # get a workflow as example
#' workflow <- fastgenomicsRclient::get_edit_model_of_workflow(connection, "wf_abc")
#' fastgenomicsRclient::save_workflow_as_file(workflow, "edit_mode.json")
save_workflow_as_file <- function(workflow, path){
  if (!is(workflow, "FGResponse"))
  {
    stop("the given workflow is not a FGResponse")
  }

  if (!workflow@DataType == "workflow") {
    stop("the given object has not the datatype workflow")
  }
  readr::write_file(httr::content(workflow@response, "text"), path)

  message(stringr::str_interp("saved workflow to ${path}"))
}
