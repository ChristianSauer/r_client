library(stringr)
library(zip)

get_default_headers <- function(connection){
  client_version = getNamespaceVersion("fastgenomicsRclient")
  ua <- stringr::str_interp("FASTGenomicsRClient Version ${client_version}")
  headers = httr::add_headers(useragent = ua, Authorization = connection@bearer_token)
}

zip_file = function(file){
    file <- normalizePath(file)
    file.zip <- paste(c(file, "zip"), collapse=".")
    message(stringr::str_interp("compressing file '${file}', this may take a while..."))
    oldwd = getwd()

    tryCatch({
        setwd(dirname(file))
        zip::zip(file.zip, basename(file))
        if(! file.exists(file.zip) )
            stop("Could not find the compressed file ${file.zip}.")
        file.remove(basename(file))
    },
    error = stop,
    finally = {
        setwd(oldwd)
    })

    return(file.zip)
}
