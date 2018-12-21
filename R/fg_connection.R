library(methods)
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
