library(methods)
library(stringr)
library(httr)
library(jsonlite)

scopes = list("All", "Public", "Private")

get_datasets <- function(connection, scope="All"){
  if(!scope %in% scopes)
  {
    scope_str = paste(as.character(scopes), collapse=", ")
    msg = str_interp("scope is '${scope}' but should be one of: ${scope_str}")
    stop(msg)
  }

  assert_is_connection(connection)
  url <-  paste(connection@base_url, "dataset/api/v1/datasets", sep="")
  client_version = getNamespaceVersion("fastgenomicsRclient")
  ua <- str_interp("FASTGenomicsRClient Version ${client_version}")
  headers = httr::add_headers(useragent = ua, Authorization = connection@bearer_token)
  response <- httr::GET(url, headers, query=list(scope=scope,includeHateoas="true" ))
  stop_for_status(response)
  # todo query! # hateoas

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- "" #  we do have multiple objects, so no id
  result <- new("FGResponse", path = url, content = parsed, DataType="List ofDatasets", Id=dataset_id )

  return(result)
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

setMethod("show", "FGResponse", function(object) {
  cat(is(object)[[1]], "\n",
      "  DataType: ", object@DataType, "\n",
      "  Id:  ", object@Id, "\n",
      sep = ""
  )
})
