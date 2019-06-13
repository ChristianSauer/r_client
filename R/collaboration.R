library(methods)
library(stringr)
library(httr)
library(jsonlite)
library(lubridate)
library(curl) # not a dep, imported through httr

#' Get all shares
#'
#' Pagination is handled in the client
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' shares <- fastgenomicsRclient::get_shares(connection)
#' print(shares@content) # all shares available to you
get_shares <- function(connection){
  url <-  paste(connection$base_url, "collaboration/api/v1/shares?take=32000", sep = "")

  assert_is_connection(connection)

  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers)
  httr::stop_for_status(response)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

  id <- "" #  we do have multiple objects, so no id
  result <- new("FGResponse", path = url, content = parsed[["embedded"]], DataType = "share", Id = id, response = response )

  return(result)
}

#' Create a new share
#'
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param dataset_id The id of the dataset to be shared. You must own this dataset. Should start with ds_
#' @param message The message to be presented to the recipients
#' @param license The license to be presented to the recipients
#' @param send_to List of email addresses
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' share <- create_share(conn, "dts_0f91c4f034e1471fa25f1078d23e1a97", message = "no msg", license = "license", send_to = list("a2@example.com"))
#' print(share@content) # the new share
create_share <- function(connection, dataset_id, message, license, send_to){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/shares", sep = "")

  body = list(message = message,
              license = license,
              send_to = send_to,
              subject = list(
                id = dataset_id,
                type = "dataset"
                 )
              )

  response <- httr::POST(url, headers, body = body, encode = "json")
  return(parse_response(response, "share"))
}

#' Delete a share
#'
#' Note that all recipients must decline or have their access revoked before the share can be deleted.
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param share_id The share to be deleted.
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' share <- create_share(conn, "dts_0f91c4f034e1471fa25f1078d23e1a97", message = "no msg", license = "license", send_to = list("a2@example.com"))
#' delete_share(conn, share@Id)
delete_share <- function(connection, share_id){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/shares/", share_id, sep = "")

  response <- httr::DELETE(url, headers)

  if (response[["status_code"]] != 200)
  {
    return(TRUE)
  }
  else
  {
    return(parse_response(response, "share"))
  }

  return(TRUE)
}

#' Revoke a share for a recipient
#'
#' If a recipient is unknown, a NotFound Message will be generated
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param share_id The id of the share
#' @param recipient_email The email of the recipient
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' share <- create_share(conn, "dts_0f91c4f034e1471fa25f1078d23e1a97", message = "no msg", license = "license", send_to = list("a2@example.com"))
#' revoke_share(conn, share@Id, "a2@example.com")
revoke_share <- function(connection, share_id, recipient_email){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/shares/", share_id, "/revoke", sep = "")

  body = list(eMail = recipient_email)

  response <- httr::PUT(url, headers, body = body, encode = "json")

  if (response[["status_code"]] == 400 || length(response[["content"]]) == 0)
  {
    return(TRUE)
  }
  else
  {
    return(parse_response(response, "share"))
  }
}


#' Get a share
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param share_id The id of the share
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' share <- create_share(conn, "dts_0f91c4f034e1471fa25f1078d23e1a97", message = "no msg", license = "license", send_to = list("a2@example.com"))
#' get_share(conn, share@Id)
get_share <- function(connection, share_id){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/shares/", share_id, sep = "")

  response <- httr::GET(url, headers)

  return(parse_response(response, "share"))

}

#' Get an offer
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param offer_id The id of the offer
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' get_offer(conn, "offer")
get_offer <- function(connection, offer_id){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/offers/", offer_id, sep = "")

  response <- httr::GET(url, headers)

  return(parse_response(response, "share"))

}


#' Get up to 100 offers from the client
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param take How many objects to take
#' @param skip How many objects to skip
#'
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' offers <- fastgenomicsRclient::get_offers(connection)
#' print(offers@content) # all offers available to you
get_offers <- function(connection, take=100, skip=0) {
  url <-  paste(connection$base_url, "collaboration/api/v1/offers?take=", take, "&skip=", skip,  sep = "")

  assert_is_connection(connection)

  headers <- get_default_headers(connection)
  response <- httr::GET(url, headers)
  httr::stop_for_status(response)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

  id <- "" #  we do have multiple objects, so no id
  result <- new("FGResponse", path = url, content = parsed[["embedded"]], DataType = "offer", Id = id, response = response )

  return(result)
}

#' Accept an offer
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param offer_id The id of the offer
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' accept_offer(conn, "of_123")
accept_offer <- function(connection, offer_id){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/offers/", offer_id, "?answer=", "Accept", sep = "")

  response <- httr::PUT(url, headers)

  return(parse_response(response, "offer"))
}

#' Decline an offer
#'
#' @param connection The connection to be used, call \code{\link{connect}} to obtain one.
#' @param offer_id The id of the offer
#' @return A FGResponse object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
#' decline_offer(conn, "of_123")
decline_offer <- function(connection, offer_id){
  headers <- get_default_headers(connection)
  url <-  paste(connection$base_url, "collaboration/api/v1/offers/", offer_id, "?answer=", "Decline", sep = "")

  response <- httr::PUT(url, headers)

  return(parse_response(response, "offer"))
}
