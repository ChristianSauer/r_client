#' Information about FG Errors
#'
#' @slot content the response
#' @slot path the url
#' @slot validation_errors a list of validation errors. Fixe these errors and try your operation again.
#'
#' @return class FGErrorResponse
#' @export
#'
#' @examples
#' None
setClass("FGErrorResponse",
         slots = c(
           content  = "list",
           path  = "character",
           validation_errors = "list"
         )
)
