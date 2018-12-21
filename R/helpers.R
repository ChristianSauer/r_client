library(stringr)

get_default_headers <- function(connection){
  client_version = getNamespaceVersion("fastgenomicsRclient")
  ua <- str_interp("FASTGenomicsRClient Version ${client_version}")
  headers = httr::add_headers(useragent = ua, Authorization = connection@bearer_token)
}
