library(methods)
library(stringr)
library(httr)
library(jsonlite)
library(installr)

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
    msg = str_interp("dataset_id cannot be empty!")
    stop(msg)
  }

  url <- paste(connection@base_url, "dataset/api/v1/datasets/", dataset_id, sep="")

  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers, query=list(includeHateoas="true"))

  if (response["status_code"] == 404) {
    stop(str_interp("The dataset '${dataset_id}' was not found on the server"),
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
    msg = str_interp("No download link found, something is wrong. Please contact us.")
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
    optional_data <<- get_data_from_FGDatasetUploadParameters(optional_parameters, connection)
  }

  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "dataset/api/v1/datasets", sep="")

  body = list(
    matrix  = upload_file(matrix_path),
                          title = title,
                          description = description,
                          short_description = short_description,
                          organism_id = organism_id,
                          matrix_format= matrix_format,
                          gene_nomenclature=gene_nomenclature)

  body = c(body, optional_data)

  response <- httr::POST(url, headers, body = body )

  # todo optional parameters
  # todo poll upload status
  # todo handle error ase
  browser()
  stop_for_status(response)
  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- parsed[["dataset_id"]]
  result <- new("FGResponse", path = url, content = parsed, DataType="Dataset", Id=dataset_id )
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

check_FGDatasetUploadParameters <- function(object) {
  browser()
  errors <- character()
  # technology check needs the connection, chec therefore done later.
  # same for current_normalization_status

  if (object@batch_column != "" && object@cell_metadata == "") {
    msg <- stringr::str_interp("If batch_column is set, you need to provide a file containing cell_metatadata, too!")
    errors <- c(errors, msg)
  }


  if (object@cell_metadata != "" && !file.exists(object@cell_metadata)) {
    msg <- stringr::str_interp("The given cell metadata file '${object@cell_metadata}' doe not exist!")
    errors <- c(errors, msg)
  }

  if (object@gene_metadata != "" && !file.exists(object@gene_metadata)) {
    msg <- stringr::str_interp("The given gene metadata file '${object@gene_metadata}' doe not exist!")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

setClass("FGResponse",
         slots = c(
           content  = "list",
           path  = "character",
           response   = "list",
           DataType = "character",
           Id = "character"
         )
)

setClass("FGDatasetUploadParameters",
         slots = c(
           license  = "character",
           Web_link  = "character",
           notes   = "character",
           citation = "character",
           technology = "character",
           batch_column = "character",
           current_normalization_status = "character",
           cell_metadata = "character",
           gene_metadata = "character"
         ),
         validity = check_FGDatasetUploadParameters

) -> FGDatasetUploadParameters

setMethod("initialize", "FGDatasetUploadParameters",
          function(.Object, ...) {
            .Object@license  = ""
            .Object@Web_link  = ""
            .Object@notes   = ""
            .Object@citation = ""
            .Object@technology = ""
            .Object@batch_column = ""
            .Object@current_normalization_status = ""
            .Object@cell_metadata = ""
            .Object@gene_metadata = ""

            .Object <- callNextMethod()
            return(.Object)
          })

get_data_from_FGDatasetUploadParameters <- function(object, connection){
              data <- list()

              if (!object@license == "")
              {
                data[license] = object@license
              }

              if (!object@Web_link == "")
              {
                data[Web_link] = object@Web_link
              }

              if (!object@notes == "")
              {
                data[notes] = object@notes
              }

              if (!object@citation == "")
              {
                data[citation] = object@citation
              }

              if (!object@technology == "")
              {
                technologies <- get_valid_technologies(connection)

                if (!object@technology %in% technologies)
                {
                  str = paste(as.character(technologies), collapse=", ")
                  stop(stringr::str_interp("The Technology '${object@technology} is unknown. Choose one of: ${str}' "))
                }

                data[technology] = object@technology
              }

              if (!object@batch_column == "")
              {
                data[batch_column] = object@batch_column
              }

              if (!object@current_normalization_status == "")
              {
                cns <- get_valid_current_normalization_status(connection)

                if (!object@current_normalization_status %in% cns)
                {
                  str = paste(as.character(cns), collapse=", ")
                  stop(stringr::str_interp("The current_normalization_status '${object@current_normalization_status} is unknown. Choose one of: ${str}' "))
                }

                data[current_normalization_status] = object@current_normalization_status
              }

              if (!object@cell_metadata == "")
              {
                data[cell_metadata] = httr::upload_file(object@cell_metadata)
              }

              if (!object@gene_metadata == "")
              {
                data[gene_metadata] = httr::upload_file(object@gene_metadata)
              }

              return(data)
          }

setMethod("show", "FGResponse", function(object) {
  cat(is(object)[[1]], "\n",
      "  DataType: ", object@DataType, "\n",
      "  Id:  ", object@Id, "\n",
      sep = ""
  )
})
