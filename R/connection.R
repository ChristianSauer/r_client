connect <- function(bearer_token) {
  if (bearer_token=="") {
    stop("The Bearer Token cannot be empty!")
  }

  if (!startsWith(bearer_token, "Bearer ey"))
  {
    stop("The Bearer Token should look like 'Bearer ey.....'")
  }
}
