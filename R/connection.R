#' Get a FastGenomics connection object to a specific FASTGenomics instance
#'
#' Note that our tokens have an expiration date, e.g. after 6 hours you will need to obtain a new Bearer token an create a new connection object.
#' WARNINIG: NEVER commit passwords or Tokens to a GIT repository or share them in any way! The token can be used to do any action on your behalf.
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param bearer_token The API token of your user, you can get it here: {base_url}/ids/Account/ApiTokenLogin
#'
#' @return a connection object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Beaer ey...")
connect <- function(base_url, bearer_token) {
  if (!endsWith(base_url, "/")){
      base_url = paste(base_url, "/", sep="")
      message("base_url is missing '/' at the end, appending...")
  }

  success <- httr::GET(base_url) # throws error if url invalid
  if (!success["status_code"] == 200)
  {
    stop("The base_url is invalid")
  }

  result =  new("FGConnection", base_url = base_url, bearer_token = bearer_token)
  return(result)
}
