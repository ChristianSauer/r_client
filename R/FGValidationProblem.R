library(stringr)
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
setClass("FGValidationProblem",
         slots = c(
           errors  = "list",
           title = "character",
           type = "character",
           status = "integer",
           detail = "character",
           instance = "character"
         )
)

setMethod("show", "FGValidationProblem", function(object) {
  title <- object@title
  errors <- object@errors
  print(stringr::str_interp("${title}: ${errors}"))
})
