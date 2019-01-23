library(stringr)
library(zip)

get_default_headers <- function(connection){
  client_version = getNamespaceVersion("fastgenomicsRclient")
  ua <- stringr::str_interp("FASTGenomicsRClient Version ${client_version}")
  headers = httr::add_headers(useragent = ua, Authorization = connection@bearer_token)
}

zip_file = function(file){
    message(stringr::str_interp("compressing file '${file}', this may take a while..."))
    zip_file <- paste(c(normalizePath(file), "zip"), collapse=".")
    oldwd = getwd()

    tryCatch({
        setwd(dirname(file))
        zip::zip(zip_file, basename(file))
        if(! file.exists(zip_file) )
            stop("Could not find the compressed file.")
        file.remove(basename(file))
    },
    error = stop,
    finally = {
        setwd(oldwd)
    })

    return(zip_file)
}
