library(methods)
library(stringr)
library(jose)
library(jsonlite)
library(stringr)
library(httr)
library(R6)

#' A FGConnection
#'
#'  Make sure that you never share an environment which contains such an object.
#'
#' @slot base_url the URL.
#' @slot pat the personal access token.
#' @slot the email address of the account
#'
#' @return class FGConnection
#' @export
#'
#' @examples
#' None
FGConnection <- R6Class(
  "FGConnection",
  list(
    base_url = "",
    pat = "",
    email = "",
    get_bearer_token = function() {

      if (private$only_bearer_token)
      {
        if (check_if_token_is_not_expired(private$bearer_token)) {
          stop("Your bearer token is expiring soon. Please obtain a new one and create a new connection. Use PATs to avoid this problem.")
        }

        return(private$bearer_token)
      }

      is_empty <- private$bearer_token == ""
      if (is_empty || check_if_token_is_not_expired(private$bearer_token))
      {
        if (self$pat != "") {
          pat <- self$pat # the pat was directly provided
        }
        else
        {
          pat <- get_pat(self$base_url, self$email) # get the pat from the keyring
        }

        url <- paste(self$base_url, "ids/api/v1/token/pat", sep = "")
        response <-
          httr::POST(
            url,
            body = list(
              Email = self$email,
              PersonalAccessToken = pat
            ),
            encode = "json"
          )
        httr::stop_for_status(response)
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        private$bearer_token <- parsed[["access_token"]]
      }

      return(private$bearer_token)
    },
    get_token_lifetime = function(){
       token <- self$get_bearer_token()
       data <- get_data_from_token(token)
       exp <- data[["exp"]]
       date <- as.POSIXct(exp, origin = "1970-01-01")
       date <- date - Sys.time()
       return(date)
    },
    initialize = function(base_url, pat, email, bt = "") {
      stopifnot(is.character(base_url), length(base_url) == 1, base_url != "")
      stopifnot(is.character(email), length(email) == 1, email != "")

      self$base_url <- base_url
      self$pat <- pat
      self$email <- email
      private$only_bearer_token <- bt != ""
      private$bearer_token <- bt
    },
    print = function(...) {
      cat("FGConnection: \n")
      cat("  Server: ", self$base_url, "\n", sep = "")
      cat("  Email: ", self$email, "\n", sep = "")
      cat("  Time until token refresh (h):  ", self$get_token_lifetime(), "\n", sep = "")
      cat("  Insecure: ",  self$pat != "", " (if true, do not store this object in the history)", "\n", sep = "")
      cat("  Bearer Token only: ",  private$only_bearer_token != "", "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    bearer_token = "",
    only_bearer_token = FALSE
  )
)


assert_is_connection <- function(connection) {
  if (!is(connection, "FGConnection"))
  {
    stop(
      "the connection is invalid, call fastgenomicsRClient::connect to obtain a valid connection"
    )
  }
}

get_data_from_token <- function(bearer_token) {
  jwt <- stringr::str_replace(bearer_token, "Bearer ", "")
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])

  payload <- rawToChar(jose::base64url_decode(strings[2]))
  data <- jsonlite::fromJSON(payload, simplifyVector = FALSE)

  return(data)
}

check_if_token_is_not_expired <- function(bearer_token) {
  data <- get_data_from_token(bearer_token)
  # refresh tokens if the token has less than two hours left
  if (data[["exp"]] < as.integer(as.POSIXct(Sys.time() + 2*60*60)))
  {
    return(TRUE)
  }
  return(FALSE)
}
