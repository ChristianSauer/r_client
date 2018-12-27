setClass("FGErrorResponse",
         slots = c(
           content  = "list",
           path  = "character",
           validation_errors = "list"
         )
)
