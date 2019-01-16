library(httr)
setOldClass("response")
#' A FG response
#'
#' @slot content The data
#' @slot path The url used to get the data
#' @slot response the raw response.
#' @slot DataType The type of data
#' @slot Id The id of the object, if the content is a list, this is blank
#'
#' @return class FGResponse
#' @export
#'
#' @examples
#' None
setClass("FGResponse",
         slots = c(
           content  = "list",
           path  = "character",
           response   = "response",
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

#' Parses a raw httr response into an FGResponse representing an
#' object of type datatype.  If the httr response contains a parsable
#' error the function will return an FGErrorResponse instead.
#'
#' @param response The httr response
#' @param datatype
#'
#' @return class FGResponse or class FGErrorResponse
parse_response <- function(response, datatype){

    if (response["status_code"] == 422) {
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        validation_errors <- parsed[["validation_errors"]]
        warning(stringr::str_interp("Upload of dataset failed due to these errors: ${validation_errors}"),
                call. = FALSE)
        fgresponse <- new("FGErrorResponse",
                          path=url, content = parsed, validation_errors=validation_errors)
    }
    else if(response["status_code"] == 400){
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        message = parsed[["message"]]
        error_code = parsed[["error_code"]]
        warning(stringr::str_interp("Upload of dataset failed due to an error: ${message}"),
             call. = FALSE)
        fgresponse <- new("FGErrorResponse",
                          path=url, content=parsed, validation_errors=paste(error_code, message))
    }
    else {
        httr::stop_for_status(response) # abort on all other errors
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        fgresponse <- new("FGResponse", content=parsed,
                          path=response$url, response=response,
                          DataType=datatype, Id=parsed[["id"]])
    }
    return(fgresponse)
}
