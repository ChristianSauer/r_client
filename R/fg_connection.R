library(methods)
library(stringr)
library(jose)
library(jsonlite)
library(stringr)

setClass("FGConnection",
         slots = c(
           base_url = "character",
           bearer_token = "character"
         )
)

assert_is_connection <- function(connection){
  if (!is(connection, "FGConnection"))
  {
    stop("the connection is invalid, call fastgenomicsRClient::connect to obtain a valid connection")
  }
}

assert_token_is_not_expired <- function(connection){
  jwt <-stringr::str_replace(connection@bearer_token, "Bearer ", "")
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])

  payload <- rawToChar(jose::base64url_decode(strings[2]))
  data <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  if (data[["exp"]] < as.integer(as.POSIXct(Sys.time())))
  {
    Stop("Your Bearer Token has expired! Please obtain a new Bearer Token")
  }
}
