#' A helper function that creates a template for the analysis.  A
#' template is an internal representation of the analysis that is yet
#' to be started.  Aside from the combination of dataset_id and
#' workflow_id it contains some additional metadata like title,
#' abstract and description.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param dataset_id dataset to be anlayzed
#' @param workflow_id workflow to be used
#' @param title title of the analysis
#' @param abstract short abstract of the analysis
#' @param description longer description
#'
#' @return class FGResponse
#'
#' @examples
#' None
create_analysis_template <- function(connection, dataset_id, workflow_id, title, abstract, description){
    assert_is_connection(connection)

    headers <- get_default_headers(connection)
    url <- paste(connection$base_url, "analysis/api/v1/analysistemplates", sep = "")
    body <- list(title = title, abstract = abstract,
                 description = description, dataset_id = dataset_id,
                 workflow_id = workflow_id)

    response <- httr::POST(url, headers, body = body, encode = "json")
    return(parse_response(response, "analysistemplate"))
}

#' This function orders an analysis to run on FASTGenomics.  Aside
#' from the dataset id and the workflow id you will need to specify a
#' title, abstract and optionally a description.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @return class FGResponse
#'
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Bearer ey...")
#' analysis <- fastgenomicsRclient::run_analysis(connection, "dts_xxxx", "wf_xxx", title="Some title", abstract="Some abstract")
#' status <- fastgenomicsRclient::poll_analysis_until_validated(connection, analysis)
run_analysis <- function(connection, dataset_id, workflow_id, title, abstract, description=""){
    assert_is_connection(connection)

    if (!startsWith(workflow_id, "wf_"))
        stop(stringr::str_interp("The workflow_id '${workflow_id}' must start with wf_"))

    if (!startsWith(dataset_id, "dts_"))
        stop(stringr::str_interp("The dataset_id '${dataset_id}' must start with dts_"))

    template = create_analysis_template(connection, dataset_id,
                                        workflow_id, title, abstract,
                                        description)

    if (!is(template, "FGResponse"))
        return(template)

    headers <- get_default_headers(connection)
    url <- paste(connection$base_url, "analysis/api/v1/analyses/compute", sep = "")
    body <- list(analysis_template_id = template@Id, name = title)

    response <- httr::POST(url, headers, body = body, encode = "json")
    return(parse_response(response, "analysis"))
}

#' Waits for analysis to complete.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param analysis Either the id of an analysis or an FGResponse object containing submitted analysis
#' @param poll_intervall The time to wait for a new status update in seconds
#'
#' @return TRUE if the analysis succeeded, otherweise FALSE
#' @export
poll_analysis_until_completed <- function(connection, analysis, poll_intervall=10){
    assert_is_connection(connection)

    if (is(analysis, "FGResponse"))
    {
        dtype <- analysis@DataType
        if (!dtype == "analysis") {
            stop(stringr::str_interp("Only FgResponse with a DataType of 'dataset' can be polled! This is a ${dtype}"))
        }
        analysis_id <- analysis@Id
    }
    else if (is.character(analysis))
        analysis_id = analysis
    else
        stop(stringr::str_interp("analysis must be either a character vector or a FGResponse object."))

    headers <- get_default_headers(connection)
    url <-  paste(connection$base_url, "analysis/api/v1/analyses/", analysis_id, "/computation_status", sep = "")

    n_tasks_finished = 0
    response <- httr::GET(url, headers)
    httr::stop_for_status(response)
    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    ## message(parsed$initialization_log)

    while (TRUE) {
        Sys.sleep(poll_intervall)
        response <- httr::GET(url, headers)
        httr::stop_for_status(response)
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

        if (parsed$execution_state == "Failed") {
            message("Execution failed")
            return(FALSE)
        }
        else if (parsed$execution_state == "Finished") {
            message("Execution successful")
            return(TRUE)
        }

        if (n_tasks_finished < length(get_finished(parsed$tasks))) {
            tasks_finished = get_finished(parsed$tasks)
            n_tasks_finished = n_tasks_finished + 1
            message(task_to_text(tasks_finished[[n_tasks_finished]]))
        }
    }
}

task_to_text <- function(task){
    paste(task$end_time, task$name, task$execution_state)
}

get_finished <- function(tasks){
    Filter(function(task) task$execution_state == "Finished", tasks)
}
