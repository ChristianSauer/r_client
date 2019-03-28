library(httr)
library(stringr)
library(keyring)

#' Get a FastGenomics connection object to a specific FASTGenomics instance
#'
#' WARNINIG: NEVER commit passwords or Tokens to a GIT repository or share them in any way! The token can be used to do any action on your behalf.
#' In the background, your PAT is used.
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#'
#' @return a connection object
#' @export
#'
#' @examples
#' fastgenomicsRclient::save_personal_access_token("https://fastgenomics.org/", "user@example.com")
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com")
connect <- function(base_url, email) {
  if (!endsWith(base_url, "/")) {
      base_url = paste(base_url, "/", sep = "")
      message("base_url is missing '/' at the end, appending...")
  }

  if (email == "" ||  !is.character(email)) {
    stop("The email must be a non empty string")
  }

  service_name <- get_service_name(base_url, email)
  if (nrow(keyring::key_list(service = service_name)) == 0)
  {
    stop("The base url has no personal access token associated. Please call save_personal_access_token first")
  }

  pat <- keyring::key_get(service_name)

  is_valid <- pat_is_valid(base_url, email, pat)
  if (!is_valid)
  {
      tryCatch(
          keyring::key_delete(service_name),
          error = function(e) NULL)
    stop("The PAT was not valid anymore. Please create a new PAT, call save_personal_access_token and try again.")
  }

  result <- FGConnection$new(base_url = base_url, pat = pat, email = email)
  return(result)
}

#' Get a FastGenomics connection object to a specific FASTGenomics instance.
#' This method is less secure than using connect.
#' Use this method only if save_personal_access_token does not work, e.g. because your keyring does not work or you work in a Jupyter Notebook.
#'
#' WARNINIG: NEVER commit passwords or Tokens to a GIT repository or share them in any way! The token can be used to do any action on your behalf.
#' In the background, your PAT is used.
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#' @param pat Your PAT, NEVER share this or store it in your history etc.
#'
#' @return a connection object
#' @export
#'
#' @examples
#' fastgenomicsRclient::save_personal_access_token("https://fastgenomics.org/", "user@example.com")
#' connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com")
connect_with_pat <- function(base_url, email, pat) {
  if (!endsWith(base_url, "/")) {
    base_url = paste(base_url, "/", sep = "")
    message("base_url is missing '/' at the end, appending...")
  }

  if (email == "" ||  !is.character(email)) {
    stop("The email must be a non empty string")
  }

  if (pat == "" ||  !is.character(pat)) {
    stop("The pat must be a non empty string")
  }

  result <- FGConnection$new(base_url = base_url, pat = pat, email = email)
  return(result)
}


#' Store a Personal Access Token (PAT) on your system
#'
#' You will be asked for a PAT, please generate it at: <base_url>/ids/Manage/ManagePats
#' The PAT is stored securely in a keyring, which can only be accessed by your user.
#' WARNINIG: NEVER commit passwords, Tokens or PATs to a GIT repository or share them in any way! The token can be used to do any action on your behalf.
#'
#' @param base_url The url of the instance, e.g. https://fastgenomics.org/
#' @param email The email address of your account
#'
#' @return Nothing
#' @export
#'
#' @examples
#' fastgenomicsRclient::save_personal_access_token("https://fastgenomics.org/", "user@example.com")
save_personal_access_token <- function(base_url, email) {
  if (!endsWith(base_url, "/")) {
    base_url = paste(base_url, "/", sep = "")
  }

  success <- httr::GET(base_url) # throws error if url invalid

  if (!success["status_code"] == 200)
  {
    stop("The base_url is invalid")
  }

  if (email == "" ||  !is.character(email)) {
    stop("The email must be a non empty string")
  }

  service_name <- get_service_name(base_url, email)
  keyring::key_set(service_name, keyring = "")

  pat <- keyring::key_get(service_name, keyring = "")
  is_valid <- pat_is_valid(base_url, email, pat)

  if (!is_valid)
  {
      tryCatch(
          keyring::key_delete(service_name, keyring = ""),
          error = function(e) NULL)
    stop("The PAT was not valid! Please provide a valid PAT")
  }
}

pat_is_valid <- function(base_url, email, pat)
{
  url <- paste(base_url, "ids/api/v1/token/pat", sep = "")
  response <- httr::POST(url, body = list(Email = email, PersonalAccessToken = pat), encode = "json")

  if (response["status_code"] != 200) {
    details <- httr::content(response, "text")

    tryCatch(
        keyring::key_delete(service_name, keyring = ""),
        error = function(e) NULL)

    message("The PAT does not appear to be valid. Error Message:",
         call. = FALSE
    )
    message(details)
    return(FALSE)
  }

  return(TRUE)
}

get_service_name <- function(base_url, email){
  rv <- stringr::str_interp("fg_pat_${base_url}_${email}")
  return(rv)
}
