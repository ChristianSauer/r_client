library(stringr)
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
