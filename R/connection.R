library(httr)
library(stringr)
library(keyring)

#' Get a FastGenomics connection object to a specific FASTGenomics instance
#'
#' If you did not provide a Personal Access Token (PAT) for this URL / user combination, you will be prompted to enter your PAT.
#' The PAT itself is stored in your systems keyring and not in your R History.
#' WARNINIG: NEVER store your PAT in your R history or in Source Control.
#' Anybody who has the PAT can take any action on FASTGenomics you can make.
#' More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#'
#' @return a connection object
#' @export
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com")
connect <- function(base_url, email) {
  base_url <- validate_base_url(base_url)

  validate_email(email)

  service_name <- get_service_name(base_url, email)
  if (nrow(keyring::key_list(service = service_name)) == 0)
  {
    keyring::key_set(service_name)
  }

  get_pat(base_url, email) # we call this once to check if we have a valid pat

  result <- FGConnection$new(base_url = base_url, pat = "", email = email)
  return(result)
}

#' Get a FastGenomics connection object to a specific FASTGenomics instance.
#' This method is less secure than using connect since the PAT will end in your R History
#'
#' If you use this method, please delete the FgConnection object when you are done.
#' Use this method only if connect does not work, e.g. because your keyring does not work or you work in a Jupyter Notebook.
#' WARNINIG: NEVER store your PAT in your R history or in Source Control.
#' Anybody who has the PAT can take any action on FASTGenomics you can make.
#' More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#' @param pat Your PAT, NEVER share this or store it in your history etc.
#'
#' @return a connection object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com", "pat")
connect_with_pat_insecure <- function(base_url, email, pat) {
  base_url <- validate_base_url(base_url)

  validate_email(email)

  if (pat == "" ||  !is.character(pat)) {
    stop("The pat must be a non empty string")
  }

  result <- FGConnection$new(base_url = base_url, pat = pat, email = email)
  return(result)
}

#' Get a FastGenomics connection object to a specific FASTGenomics instance
#'
#' This methods uses bearer tokens. Good for demonstrations but needs frequent manual refreshment of the bearer token. Prefer connect to use a PAT.
#' If you use this method, please delete the FgConnection object when you are done.
#' More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#' @param bearer_token Your Bearer Token, NEVER share this or store it in your history etc. Should look like 'Bearer ey'
#'
#' @return a connection object
#' @export
#'
#' @examples
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com", "Bearer ey...")
connect_with_bearer_token <- function(base_url, email, bearer_token) {
  base_url <- validate_base_url(base_url)

  validate_email(email)

  if (bearer_token == "" ||  !is.character(bearer_token)) {
    stop("The bearer_token must be a non empty string")
  }

  if (!stringr::str_starts(bearer_token, "Bearer ey")) {
    stop("The bearer_token should start with 'Bearer ey'")
  }

  bearer_token <- str_replace(bearer_token, "Bearer ", "") # will be added by inf again

  result <- FGConnection$new(base_url = base_url, pat = "", email = email, bt = bearer_token)
  return(result)
}

validate_base_url <- function(base_url)
{
  if (base_url == "" ||  !is.character(base_url)) {
    stop("The base_url must be a non empty string")
  }

  if (!endsWith(base_url, "/")) {
    base_url = paste(base_url, "/", sep = "")
  }

  success <- httr::GET(base_url) # throws error if url invalid

  if (!success["status_code"] == 200)
  {
    stop(stringr::str_interp("The base_url '${base_url}' is invalid. Please check the url and try again."))
  }

  return(base_url)
}

pat_is_valid <- function(base_url, email, pat) {
  url <- paste(base_url, "ids/api/v1/token/pat", sep = "")
  response <- httr::POST(url, body = list(Email = email, PersonalAccessToken = pat), encode = "json")

  if (response["status_code"] != 200) {
    details <- httr::content(response, "text")

    tryCatch(
        keyring::key_delete(service_name, keyring = ""),
        error = function(e) NULL)

    message("The PAT does not appear to be valid. Error Message:\n")
    message(details)
    return(FALSE)
  }

  return(TRUE)
}

get_service_name <- function(base_url, email){
  rv <- stringr::str_interp("fg_pat_${base_url}_${email}")
  return(rv)
}

validate_email <- function(email){
  if (email == "" ||  !is.character(email)) {
    stop("The email must be a non empty string")
  }
}

get_pat <- function(base_url, email){
  service_name <- get_service_name(base_url, email)
  if (nrow(keyring::key_list(service = service_name)) == 0)
  {
    stop("The base url has no personal access token associated. Please call connect")
  }

  pat <- keyring::key_get(service_name)

  is_valid <- pat_is_valid(base_url, email, pat)
  if (!is_valid)
  {
    tryCatch(
      keyring::key_delete(service_name),
      error = function(e) NULL)
    stop("The PAT is not valid. Please check the url, username and PAT.")
  }

  return(pat)
}
