library(httr)
setOldClass("response")
#' A FG response after a successful request
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


#' Validation error
#'
#' @slot content the response
#' @slot path the url
#' @slot validation_errors a list of validation errors. Fixe these errors and try your operation again.
#'
#' @return class FGErrorModelResponse
#' @export
#'
#' @examples
#' None
setClass("FGErrorModelResponse",
         slots = c(
             content  = "list",
             path  = "character",
             validation_errors = "list"
         )
         )


#' Unexpected server error
#'
#' @slot content the response
#' @slot path the url
#' @slot error_code error code
#' @slot message human readable error message
#' @slot help
#'
#' @return class FGErrorResponse
#' @export
#'
#' @examples
#' None
setClass("FGErrorResponse",
         slots = c(
             content = "list",
             path = "character",
             error_code = "character",
             message = "character",
             help = "character"
         )
         )


#' Encapsulates \href{https://tools.ietf.org/html/rfc7807}{RFC 7807 Problem details}, with the errors field added
#'
#' @slot errors List of errors, human readable
#' @slot title A short, human-readable summary of the problem type.
#' @slot type the problem type
#' @slot status The HTTP status code
#' @slot detail  human-readable explanation specific to this occurrence of the problem.
#' @slot instance A URI reference that identifies the specific occurrence of the problem.
#'
#' @return FGValidationProblem
#' @export
#'
#' @examples
#' None
setClass("FGValidationProblemResponse",
         slots = c(
             path = "character",
             content = "list",
             errors  = "list",
             title = "character",
             type = "character",
             status = "integer",
             detail = "character",
             instance = "character"
         )
         )

setMethod("show", "FGValidationProblemResponse", function(object) {
    title <- object@title
    errors <- object@errors
    print(stringr::str_interp("${title}: ${errors}"))
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
    ## errors we can parse
    if (response["status_code"] %in% c(400, 409, 422, 500)) {
        content <- jsonlite::fromJSON(httr::content(response, "text"),
                                     simplifyVector = FALSE)
        url = response$url

        if(all(c("errors", "type", "title", "status", "detail", "instance")
               %in% names(content))) {
            errors = content[["errors"]]
            detail = content[["detail"]]
            warning(stringr::str_interp("Upload of dataset failed due to a validation error: ${errors} ${detail}"),
                    call. = FALSE)
            fgresponse <- new(
                "FGValidationProblemResponse",
                path=url,
                content=content,
                errors=errors,
                title=content[["title"]],
                type=content[["type"]],
                status=content[["status"]],
                detail=detail,
                instance=content[["instance"]])
        }
        else if("validation_errors" %in% names(content)){
            validation_errors <- content[["validation_errors"]]
            warning(stringr::str_interp("Upload of dataset failed due to these errors: ${validation_errors}"),
                    call. = FALSE)
            fgresponse <- new(
                "FGErrorModelResponse",
                content=content,
                path=url,
                validation_errors=validation_errors)
        }
        else if(all(c("error_code", "message", "help") %in% names(content))) {
            message = content[["message"]]
            error_code = content[["error_code"]]
            help = content[["help"]]
            warning(stringr::str_interp("Upload of dataset failed due to an error: ${message}. ${help}"),
                    call. = FALSE)
            fgresponse <- new(
                "FGErrorResponse",
                path=url,
                content=content,
                error_code = error_code,
                message = message,
                help = help)
        }
        else
            stop("Unrecognized error type")
    }
    else {
        httr::stop_for_status(response) # abort on all other errors
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        if(!is.null(parsed[["id"]])){
            id = parsed[["id"]]
        } else if(!is.null(parsed[["dataset_id"]])){
            id = parsed[["dataset_id"]]
        } else if(!is.null(parsed[["image_name"]])){
            id = parsed[["image_name"]]
        } else {
            stop("Error parsing the response, could not find a valid 'id' field.")
        }
        fgresponse <- new("FGResponse", content=parsed,
                          path=response$url, response=response,
                          DataType=datatype, Id=id)
    }
    return(fgresponse)
}
