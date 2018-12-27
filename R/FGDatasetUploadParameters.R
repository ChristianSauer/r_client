check_FGDatasetUploadParameters <- function(object) {
  errors <- character()
  # technology check needs the connection, chec therefore done later.
  # same for current_normalization_status

  if (object@batch_column != "" && object@cell_metadata == "") {
    msg <- stringr::str_interp("If batch_column is set, you need to provide a file containing cell_metatadata, too!")
    errors <- c(errors, msg)
  }


  if (object@cell_metadata != "" && !file.exists(object@cell_metadata)) {
    msg <- stringr::str_interp("The given cell metadata file '${object@cell_metadata}' doe not exist!")
    errors <- c(errors, msg)
  }

  if (object@gene_metadata != "" && !file.exists(object@gene_metadata)) {
    msg <- stringr::str_interp("The given gene metadata file '${object@gene_metadata}' doe not exist!")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

setClass("FGDatasetUploadParameters",
         slots = c(
           license  = "character",
           web_link  = "character",
           notes   = "character",
           citation = "character",
           technology = "character",
           batch_column = "character",
           current_normalization_status = "character",
           cell_metadata = "character",
           gene_metadata = "character"
         ),
         validity = check_FGDatasetUploadParameters

)


FGDatasetUploadParameters <- function( license  = "",
                                       web_link  = "",
                                       notes   = "",
                                       citation = "",
                                       technology = "",
                                       batch_column = "",
                                       current_normalization_status = "",
                                       cell_metadata = "",
                                       gene_metadata = "") {

  new("FGDatasetUploadParameters",
      license=license,
      web_link=web_link,
      notes=notes,
      citation=citation,
      technology=technology,
      batch_column=batch_column,
      current_normalization_status=current_normalization_status,
      cell_metadata=cell_metadata,
      gene_metadata=gene_metadata)
}

get_data_from_FGDatasetUploadParameters <- function(object, connection){
  data <- list()
  if (!object@license == "")
  {
    data["license"] <- object@license
  }

  if (!object@web_link == "")
  {
    data["web_link"] <- object@web_link
  }

  if (!object@notes == "")
  {
    data["notes"] <- object@notes
  }

  if (!object@citation == "")
  {
    data["citation"] <- object@citation
  }

  if (!object@technology == "")
  {
    technologies <- get_valid_technologies(connection)

    if (!object@technology %in% technologies)
    {
      str = paste(as.character(technologies), collapse=", ")
      stop(stringr::str_interp("The Technology '${object@technology} is unknown. Choose one of: ${str}' "))
    }

    data["technology"] <- object@technology
  }

  if (!object@batch_column == "")
  {
    data["batch_column"] <- object@batch_column
  }

  if (!object@current_normalization_status == "")
  {
    cns <- get_valid_current_normalization_status(connection)

    if (!object@current_normalization_status %in% cns)
    {
      str = paste(as.character(cns), collapse=", ")
      stop(stringr::str_interp("The current_normalization_status '${object@current_normalization_status} is unknown. Choose one of: ${str}' "))
    }

    data["current_normalization_status"] <- object@current_normalization_status
  }

  if (!object@cell_metadata == "")
  {
    data[["cell_metadata"]] = httr::upload_file(object@cell_metadata)
  }

  if (!object@gene_metadata == "")
  {
    data[["gene_metadata"]] <- httr::upload_file(object@gene_metadata)
  }

  return(data)
}
