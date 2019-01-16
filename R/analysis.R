#' A helper function that creates a template for the analysis.  A
#' template is an internal representation of the analysis that is yet
#' to be started.  Aside from the combination of dataset_id and
#' workflow_id it contains some additional metadata like title,
#' abstract and description.
#'
#' @param connection
#' @param dataset_id
#' @param workflow_id
#' @param title
#' @param abstract
#' @param description
#'
#' @return class FGResponse
#'
#' @examples
#' None
create_analysis_template <- function(connection, dataset_id, workflow_id, title, abstract, description){
    assert_is_connection(connection)
    assert_token_is_not_expired(connection)

    headers <- get_default_headers(connection)
    url <- paste(connection@base_url, "analysis/api/v1/analysistemplates", sep="")
    body <- list(title=title, abstract=abstract,
                description=description, dataset_id=dataset_id,
                workflow_id=workflow_id)

    response <- httr::POST(url, headers, body=body, encode="json")
    return(parse_response(response, "analysistemplate"))
}

#' This function orders an analysis to run on FASTGenomics.  Aside
#' from the dataset id and the workflow id you will need to specify a
#' title, abstract and optionally a description.
#'
#' @param
#' @return class FGResponse
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Bearer ey...")
#' analysis <- fastgenomicsRclient::run_analysis(connection, "dts_xxxx", "wf_xxx", title="Some title", abstract="Some abstract")
#' status <- fastgenomicsRclient::poll_analysis_until_validated(connection, analysis)
run_analysis <- function(connection, dataset_id, workflow_id, title, abstract, description=""){
    assert_is_connection(connection)
    assert_token_is_not_expired(connection)

    if(!startsWith(workflow_id, "wf_"))
        stop(stringr::str_interp("The workflow_id '${workflow_id}' must start with wf_"))

    if(!startsWith(dataset_id, "dts_"))
        stop(stringr::str_interp("The dataset_id '${dataset_id}' must start with dts_"))

    template = create_analysis_template(connection, dataset_id,
                                        workflow_id, title, abstract,
                                        description)

    if(class(template)!="FGResponse")
        return(template)

    headers <- get_default_headers(connection)
    url <- paste(connection@base_url, "analysis/api/v1/analyses/compute", sep="")
    body <- list(analysis_template_id = template@Id, name=title)

    response <- httr::POST(url, headers, body=body, encode="json")
    return(parse_response(response, "analysis"))
}
