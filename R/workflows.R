get_workflows <- function(connection, scope="All"){
  url <-  paste(connection@base_url, "workflow/api/v1/workflows", sep="")

  result <- get_data_list(connection, scope, url, "workflow")

  return(result)
}

get_workflow <-  function(connection, workflow_id, fullDetail=FALSE){
  url <-  paste(connection@base_url, "workflow/api/v1/workflows/", workflow_id , sep="")

  result <- get_data(connection, workflow_id, url, "workflow", queries = list("fullDetail"=fullDetail))

  return(result)
}

get_edit_model_of_workflow <- function(connection, workflow_id){
  url <-  paste(connection@base_url, "workflow/api/v1/workflows/", workflow_id , sep="")

  result <- get_data(connection, workflow_id, url, "workflow", additional_headers = httr::accept("application/vnd.fastgenomics.editworkflow+json") )

  return(result)
}

create_workflow <- function(connection, workflow_edit_model){

}

modify_workflow <- function(connection, workflow){}
