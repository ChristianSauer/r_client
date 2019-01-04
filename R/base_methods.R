library(methods)
library(stringr)
library(httr)
library(jsonlite)

scopes = list("All", "Public", "Private")

get_data_list <- function(connection, scope, url, data_type, queries=list()){
  if(!scope %in% scopes)
  {
    scope_str = paste(as.character(scopes), collapse=", ")
    msg = stringr::str_interp("scope is '${scope}' but should be one of: ${scope_str}")
    stop(msg)
  }
  assert_token_is_not_expired(connection)
  assert_is_connection(connection)

  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers, query=c(list(scope=scope), queries))
  stop_for_status(response)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  id <- "" #  we do have multiple objects, so no id
  result <- new("FGResponse", path = url, content = parsed, DataType=stringr::str_interp("List of ${data_type}"), Id=id, response=response )
  return(result)
}

get_data <- function(connection, object_id, url, data_type, queries=list(), additional_headers=list()){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)

  headers <- get_default_headers(connection)

  headers <- c(headers, additional_headers)
  response <- httr::GET(url, headers,  query=queries)

  if (response["status_code"] == 404) {
    stop(stringr::str_interp("The ${data_type} '${object_id}' was not found on the server"),
         call. = FALSE
    )
  }

  stop_for_status(response)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  result <- new("FGResponse", path = url, content = parsed, DataType=data_type, Id=object_id, response=response )
}
