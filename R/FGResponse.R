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
           response   = "list",
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
