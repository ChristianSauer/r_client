library(stringr)
library(R.utils)

get_default_headers <- function(connection){
  client_version = getNamespaceVersion("fastgenomicsRclient")
  ua <- stringr::str_interp("FASTGenomicsRClient Version ${client_version}")
  headers = httr::add_headers(useragent = ua, Authorization = connection@bearer_token)
}

zip_file = function(file){
    file <- normalizePath(file)
    file.gz <- paste(c(file, "gz"), collapse=".")
    message(stringr::str_interp("compressing file '${file}', this may take a while..."))
    oldwd = getwd()

    tryCatch({
        setwd(dirname(file))
        gzip(filename = basename(file), destname = file.gz, remove = T)
        if(! file.exists(file.gz) )
            stop("Could not find the compressed file ${file.gz}.")
    },
    error = stop,
    finally = {
        setwd(oldwd)
    })

    return(file.gz)
}
