connect <- function(base_url, bearer_token) {
  if (bearer_token=="") {
    stop("The Bearer Token cannot be empty!")
  }

  if (!startsWith(bearer_token, "Bearer ey"))
  {
    stop("The Bearer Token should look like 'Bearer ey.....'")
  }

  if (base_url=="") {
    stop("base_url cannot be empty!")
  }

  if (!endsWith(base_url, "/")) {
    stop(paste("base_url should look like this: 'https://fastgenomics.org/', but is: ", base_url))
  }

  success <- httr::GET(base_url) # throws error if url invalid
  if (!success["status_code"] == 200)
  {
    stop("The base_url is invalid")
  }

  result =  new("FGConnection", base_url = base_url, bearer_token = bearer_token)
  return(result)
}
