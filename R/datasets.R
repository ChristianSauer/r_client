library(methods)
library(stringr)
library(httr)
library(jsonlite)
library(installr)
library(lubridate)

scopes = list("All", "Public", "Private")

get_datasets <- function(connection, scope="All"){
  if(!scope %in% scopes)
  {
    scope_str = paste(as.character(scopes), collapse=", ")
    msg = str_interp("scope is '${scope}' but should be one of: ${scope_str}")
    stop(msg)
  }
  assert_token_is_not_expired(connection)
  assert_is_connection(connection)

  url <-  paste(connection@base_url, "dataset/api/v1/datasets", sep="")
  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers, query=list(scope=scope,includeHateoas="true" ))
  stop_for_status(response)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- "" #  we do have multiple objects, so no id
  result <- new("FGResponse", path = url, content = parsed, DataType="List ofDatasets", Id=dataset_id )

  return(result)
}

get_dataset <- function(connection, dataset_id){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)

  if(dataset_id == "")
  {
    msg = stringr::str_interp("dataset_id cannot be empty!")
    stop(msg)
  }

  url <- paste(connection@base_url, "dataset/api/v1/datasets/", dataset_id, sep="")

  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers, query=list(includeHateoas="true"))

  if (response["status_code"] == 404) {
    stop(stringr::str_interp("The dataset '${dataset_id}' was not found on the server"),
      call. = FALSE
    )
 }

  stop_for_status(response)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- parsed[["dataset_id"]]
  result <- new("FGResponse", path = url, content = parsed, DataType="Dataset", Id=dataset_id )
}

download_dataset <- function(connection, dataset_id, folder_path){
  if (!dir.exists(folder_path))
  {
    msg = str_interp("The folder '${folder_path}' does not exist, please create it")
    stop(msg)
  }

  dataset <- get_dataset(connection, dataset_id)
  download_link <- ""
  for (lnk in dataset@content[["links"]]){
    rel <- lnk[["rel"]]

    if (rel == "download-dataset-complete-zip")
    {
       download_link <- lnk[["href"]]
    }
  }

  if (download_link == "")
  {
    msg = stringr::str_interp("No download link found, something is wrong. Please contact us.")
    stop(msg)
  }

 url <- paste(substr(connection@base_url, 1, nchar(connection@base_url)-1), download_link, sep="")
 headers <- get_default_headers(connection)
 headers["output"] = write_disk(file.path(folder_path, str_interp("${dataset_id}.zip")))["output"]
 response <- httr::GET(url, headers)

 stop_for_status(response)
}

create_dataset <- function(connection, title, description, short_description, organism_id, matrix_path , matrix_format, gene_nomenclature, optional_parameters=NULL)
{
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)

  gene_nomenclatures <- get_valid_gene_nomenclatures(connection)
  if (!gene_nomenclature %in% gene_nomenclatures)
  {
    str = paste(as.character(gene_nomenclatures), collapse=", ")
    stop(stringr::str_interp("The Gene Nomenclature '${gene_nomenclature} is unknown. Choose one of: ${str}' "))
  }

  if (!installr:::check.integer(organism_id))
  {
    stop(stringr::str_interp("The organism id '${organism_id}' is not an integer. Choose Homo Sapiens: 9606 Mouse: 10090"))
  }

  matrix_formats <- get_valid_matrix_formats(connection)
  if (!matrix_format %in% matrix_formats)
  {
    str = paste(as.character(matrix_formats), collapse=", ")
    stop(stringr::str_interp("The Matrix format '${matrix_format}' is unknown. Choose one of: ${str}' "))
  }

  if (!file.exists(matrix_path))
  {
    stop(stringr::str_interp("The file '${matrix_path}' does not exist. Please provide a valid file!. See https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/dataset_api.md for valid file formats "))
  }

  optional_data <- list()

  if (!is.null(optional_parameters))
  {
    if (!is(optional_parameters, "FGDatasetUploadParameters"))
    {
      stop("the optional_parameters need to be either NULL or a FGDatasetUploadParameters object. Call new('FGDatasetUploadParameters', ..) to obtain such an object.")
    }
    optional_data <- get_data_from_FGDatasetUploadParameters(optional_parameters, connection)
  }

  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "dataset/api/v1/datasets", sep="")

  body = list(
    matrix  = httr::upload_file(matrix_path),
                          title = title,
                          description = description,
                          short_description = short_description,
                          organism_id = organism_id,
                          matrix_format= matrix_format,
                          gene_nomenclature=gene_nomenclature)

  body = c(body, optional_data)

  response <- httr::POST(url, headers, body = body )

  # todo poll upload status

  if (response["status_code"] == 422) {
    # handle errors in the upload, e.g. invalid datatypes
    parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
    validation_errors <- parsed[["validation_errors"]]
    warning(stringr::str_interp("Upload of dataset failed due to these errors: ${validation_errors}"),
            call. = FALSE
    )

    error <- new("FGErrorResponse", path = url, content = parsed, validation_errors=parsed[["validation_errors"]])
    return(error)
  }

  httr::stop_for_status(response) # abort on all other errors

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- parsed[["dataset_id"]]
  result <-new("FGResponse", path = url, content = parsed, DataType="Dataset", Id=dataset_id )
  return(result)
}

poll_for_upload_to_complete <- function(connection, dataset_id, poll_intervall=10){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)
  if (is(dataset_id, "FGResponse"))
  {
    dtype <- dataset_id@DataType
    if (!dtype == "Dataset"){
      stop(stringr::str_interp("Only FgResponse with a DataType of 'Dataset' can be polled! This is a ${dtype}"))
    }
    dataset_id <- dataset_id@Id
  }

  if (!is.character(dataset_id))
  {
    stop(stringr::str_interp("dataset_id can be either a character vector or a FgResponse object."))
  }

  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "dataset/api/v1/datasets/", dataset_id, "/status", sep="")
  last_check <- lubridate::ymd("2010/03/17") # something old
  while (TRUE) {
    Sys.sleep(poll_intervall)
    response <- httr::GET(url, headers)
    httr::stop_for_status(response)

    parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)


    for (msg in parsed) {
      msg_time <- lubridate::as_datetime(msg[["timestamp"]], tz="UTC")
      if (msg_time > last_check){
        status <- msg[["status"]]
        file_name <- msg[["file_name"]]
        msg_text <- msg[["msg"]]

        if (status == "Error"){
          warning(stringr::str_interp("${msg_time} | ${status} | ${file_name} | ${msg_text}"))
        }
        else{
          message(stringr::str_interp("${msg_time} | ${status} | ${file_name} | ${msg_text}"))
        }

      }
    }

    newest_message <- parsed[[length(parsed)]]
    if (newest_message[["status"]] == "Ready")
    {
      # success!
      return(TRUE)
    }

    if (newest_message[["status"]] == "Rejected")
    {
      # error!
      warning(stringr::str_interp("There where upload errors. Call get_dataset with the id of this dataset to obtain more information."))
      return(FALSE)
    }

    last_check <- lubridate::now(tz="UTC")
  }

}

get_valid_gene_nomenclatures = function(connection){
  return(get_info(connection, "dataset/api/v1/validgenenomenclatures"))
}

get_valid_matrix_formats = function(connection){
  return(get_info(connection, "dataset/api/v1/validmatrixformats"))
}

get_valid_technologies = function(connection){
  return(get_info(connection, "dataset/api/v1/validtechnologies"))
}

get_valid_current_normalization_status = function(connection){
  return(get_info(connection, "dataset/api/v1/validcurrentnormalizationstatus"))
}

get_info <- function(connection, url){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)

  headers <- get_default_headers(connection)
  url <- paste(connection@base_url, url, sep="")
  response <- httr::GET(url, headers)
  stop_for_status(response)
  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  data = lapply(parsed, function(x){ return(x[["key"]])})
  return(data)
}
