library(methods)
library(stringr)
library(httr)
library(jsonlite)
library(lubridate)
library(curl) # not a dep, imported through httr

#' Get all apps
#'
#' This list does not contain rejected apps, only valid, usable apps
#'
#' @param connection The connection to be used, call fastgenomicsRclient::connect to obtain one.
#' @param scope Filters the apps by their scope. Possible Values are: 'All': return all apps, 'Private': Only your personal apps, 'Public': Only public apps
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' apps <- fastgenomicsRclient::get_app(connection)
#' print(apps@content) # all apps available to you
get_apps <- function(connection, scope="All"){
  url <-  paste(connection@base_url, "app/api/v1/apps", sep="")

  result <- get_data_list(connection, scope, url, "app")

  return(result)
}

#' Get an app
#'
#' @param connection The connection to be used, call fastgenomicsRclient::connect to obtain one.
#' @param app_id the id of the app or a FGResponse object
#'
#' @return class FGResponse
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' app <- fastgenomicsRclient::get_apps(connection, "abc")
#' print(app@content) # the app
get_app <- function(connection, app_id){
  url <-  paste(connection@base_url, "app/api/v1/apps/", curl::curl_escape(app_id) , sep="")

  result <- get_data(connection, app_id, url, "app")

  return(result)
}

#' Creates a new app
#'
#' Pulls an app from a docker registry (e.g. the docker hub or a private registry) and installs it in FASTGenomics. The image will be pulled by FASTGenomics
#' After submitting your app, the app will be validated on the server, this can take a while. The app is usable when the validation has been completed
#' Use poll_app_until_validated to wait for the validation to complete.
#'
#' @param connection The connection to be used, call fastgenomicsRclient::connect to obtain one.
#' @param source_image_name The name of the app image, must inlcude a tag. E.g. yourapp:latest. Must not contain the name of the registry.
#' @param image_name The name to be used in FASTGenomics, e.g. yourapp:v1. Note that your username is always prepended, e.g. someuser/yourapp:v1.
#' @param registry The registry to be used, leave blank if you use the docker hub. Do not prepend http(s), egg someregistry.example.com
#' @param username The username of the registry, leave blank if no auth is required. FG uses this information ONLY to pull the image, neither username nor password are stored permanently.
#' @param password The password of the registry, leave blank if no auth is required.
#'
#' @return An app or a validation error
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' result <- create_app(default_conn, "library/busybox:latest", registry = "registry.hub.docker.com" , image_name = "my_image_name:1", username = "a", password = "b" )
#' status <- poll_app_until_validated(connection, result) # will return FALSE, since neither image nor credentials are valid
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
    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

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

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
  dataset_id <- parsed[["image_name"]]
  result <-new("FGResponse", path = url, content = parsed, DataType="app", Id=dataset_id, response=response )
  return(result)
}

#' Waits for the validation of the app to complete.
#'
#' Messages and errors are used to show messages. If you need all messages, use fastgenomicsRclient::get_app with the id of this app
#'
#' @param connection The connection to be used, call fastgenomicsRclient::connect to obtain one.
#' @param dataset_id The app_id of the app OR a FGResponse object
#' @param poll_intervall The time to wait for a new status update in seconds
#'
#' @return TRUE if the validation succeeded, otherweise FALSE
#' @export
#'
#' @examples
#' See create_app example
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

  headers <- get_default_headers(connection)
  url <-  paste(connection@base_url, "app/api/v1/apps/", curl::curl_escape(app_id), "/upload_status", sep="")
  last_check <- lubridate::ymd("2010/03/17") # something old
  while (TRUE) {
    Sys.sleep(poll_intervall)
    response <- httr::GET(url, headers)
    httr::stop_for_status(response)

    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)


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
