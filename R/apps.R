library(methods)
library(stringr)
library(httr)
library(jsonlite)
library(installr)
library(lubridate)
library(curl) # not a dep, imported through httr

scopes = list("All", "Public", "Private")

get_apps <- function(connection, scope="All"){
  url <-  paste(connection@base_url, "app/api/v1/apps", sep="")

  result <- get_data_list(connection, scope, url, "app")

  return(result)
}

get_app <- function(connection, app_id){
  url <-  paste(connection@base_url, "app/api/v1/apps/", curl::curl_escape(app_id) , sep="")

  result <- get_data(connection, app_id, url, "app")

  return(result)
}

create_app <- function(connection, source_image_name, image_name="", registry="", username="", password=""){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)

  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "app/api/v1/apps", sep="")

  body = list(source_image_name=source_image_name)

  if (image_name != ""){
    body["image_name"] = image_name
  }


  if (registry != ""){
    body["registry"] = registry
  }


  if (username != ""){
    body["username"] = username
  }


  if (password != ""){
    body["password"] = password
  }

  response <- httr::POST(url, headers, body = body, encode = "json")
  if (response["status_code"] == 422) {
    parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)

    error <- new("FGValidationProblem",
                 errors= parsed[["errors"]],
                 title = parsed[["title"]],
                 type = parsed[["type"]],
                 status = parsed[["status"]],
                 detail = parsed[["detail"]],
                 instance = parsed[["instance"]])
    return(error)

  }

  httr::stop_for_status(response) # abort on all other errors

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)
  dataset_id <- parsed[["image_name"]]
  result <-new("FGResponse", path = url, content = parsed, DataType="app", Id=dataset_id )
  return(result)
}

poll_app_until_validated <- function(connection, app_id, poll_intervall=10){
  assert_is_connection(connection)
  assert_token_is_not_expired(connection)
  if (is(app_id, "FGResponse"))
  {
    dtype <- app_id@DataType
    if (!dtype == "app"){
      stop(stringr::str_interp("Only FgResponse with a DataType of 'app' can be polled! This is a ${dtype}"))
    }
    app_id <- app_id@Id
  }

  if (!is.character(app_id))
  {
    stop(stringr::str_interp("app_id can be either a character vector or a FgResponse object."))
  }
  browser()
  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "app/api/v1/apps/", curl::curl_escape(app_id), "/upload_status", sep="")
  last_check <- lubridate::ymd("2010/03/17") # something old
  while (TRUE) {
    Sys.sleep(poll_intervall)
    response <- httr::GET(url, headers)
    httr::stop_for_status(response)

    parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)


    for (msg in parsed[["validation_messages"]]) {
      msg_time <- lubridate::as_datetime(msg[["time_stamp"]], tz="UTC")
      if (msg_time > last_check){
        status <- msg[["status"]]
        msg_text <- msg[["message"]]

        if (status == "Error"){
          warning(stringr::str_interp("${msg_time} | ${status} | ${msg_text}"))
        }
        else{
          message(stringr::str_interp("${msg_time} | ${status} | ${msg_text}"))
        }

      }
    }

    if (parsed[["validation_status"]] == "Ready")
    {
      # success!
      return(TRUE)
    }

    if (parsed[["validation_status"]] == "Rejected")
    {
      # error!
      warning(stringr::str_interp("There where upload errors. Call get_app with the id of this app to obtain more information."))
      return(FALSE)
    }

    last_check <- lubridate::now(tz="UTC")
  }

}
