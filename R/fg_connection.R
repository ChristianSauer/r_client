library(methods)
library(stringr)
library(jose)
library(jsonlite)
library(stringr)


check_FGConnection <- function(object){
    errors <- character()
    if (object@base_url == "") {
        msg <- stringr::str_interp("URL Cannot be empty")
        errors <- c(errors, msg)
    }

    if (!startsWith(object@bearer_token, "Bearer ey"))
    {
        msg <- stringr::str_interp("The Bearer Token should look like 'Bearer ey.....'")
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}

#' A FGConnection
#'
#'  Make sure that you never share an environment which contains such an object.
#'
#' @slot base_url the URL.
#' @slot bearer_token the token.
#'
#' @return class FGConnection
#' @export
#'
#' @examples
#' None
setClass("FGConnection",
         slots = c(
             base_url = "character",
             bearer_token = "character"
         ),
         validity = check_FGConnection
         )

assert_is_connection <- function(connection){
    if (!is(connection, "FGConnection"))
    {
        stop("the connection is invalid, call fastgenomicsRClient::connect to obtain a valid connection")
    }
}

#' Helper Function to check if a FGConnection is still valid. Normally called automatically.
#'
#' @param connection The connection to be tested
#'
#' @return None
#' @export
#'
#' @examples
#' None
assert_token_is_not_expired <- function(connection){
  jwt <-stringr::str_replace(connection@bearer_token, "Bearer ", "")
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])

  payload <- rawToChar(jose::base64url_decode(strings[2]))
  data <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  if (data[["exp"]] < as.integer(as.POSIXct(Sys.time())))
  {
    stop("Your Bearer Token has expired! Please obtain a new Bearer Token")
  }
}
