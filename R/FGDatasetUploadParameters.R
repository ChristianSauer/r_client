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


#' Get the optional parameters to create a dataset
#'
#'  If you provide cell or gene metadata via a dataframe, please note that the dataframe is stored on disk in the tempdir of the current R session. As this might get cleaned on exit of R, be cautious when using this from a stored R session.
#'
#' @param license The license to be used. If the data are owned by yourself, you can use https://creativecommons.org/choose/ to choose a license. If not, you have to find the license used by the dataset, e.g. talk to your supervisor.
#' @param web_link The website of your dataset.
#' @param notes Privates notes
#' @param citation How to cite this dataset
#' @param technology The technology used to obtain this dataset. call fastgenomicsRclient::get_valid_technologies to get a list
#' @param batch_column The colum in the cell metadata which holds information about the batch. Can be blank. If not blank, cell_metadata have to be included.
#' @param current_normalization_status The current_normalization_status used to obtain this dataset. call fastgenomicsRclient::get_current_normalization_status to get a list
#' @param cell_metadata The path to your cell metadata file OR a dataframe, for valid formats see: https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/dataset_api.md
#' @param gene_metadata The path to your gene metadata OR a dataframe, for valid formats see: https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/dataset_api.md
#'
#' @return class FGDatasetUploadParameters
#' @export
#'
#' @examples
#' optional <- fastgenomicsRclient::FGDatasetUploadParameters(
#'                                         license ="MIT",
#'                                         technology = "Smart-Seq",
#'                                         web_link="https://example.com",
#'                                         notes="This is a TEST",
#'                                         citation="FG et al",
#'                                         batch_column="sample",
#'                                         current_normalization_status="Counts",
#'                                         cell_metadata="./cell_metadata.tsv",
#'                                         gene_metadata="./gene_metadata.tsv"  )
FGDatasetUploadParameters <- function( license  = "",
                                       web_link  = "",
                                       notes   = "",
                                       citation = "",
                                       technology = "",
                                       batch_column = "",
                                       current_normalization_status = "",
                                       cell_metadata = "",
                                       gene_metadata = "") {

  cell_metadata_path <- ""
  if (is.character(cell_metadata)){
    cell_metadata_path <- cell_metadata
  }
  else if (is.data.frame(cell_metadata))
  {
    cell_metadata_path <- get_df_as_file(df, "cell_metadata.csv")
  }
  else
  {
    stop("the given cell_metadata is neither a file path nor a dataframe!")
  }

  gene_metadata_path <- ""
  if (is.character(gene_metadata)){
    gene_metadata_path <- gene_metadata
  }
  else if (is.data.frame(gene_metadata))
  {
    gene_metadata_path <- get_df_as_file(df, "gene_metadata.csv")
  }
  else
  {
    stop("the given gene_metadata is neither a file path nor a dataframe!")
  }

  new("FGDatasetUploadParameters",
      license=license,
      web_link=web_link,
      notes=notes,
      citation=citation,
      technology=technology,
      batch_column=batch_column,
      current_normalization_status=current_normalization_status,
      cell_metadata=cell_metadata_path,
      gene_metadata=gene_metadata_path)
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
    data[["gene_metadata"]] = httr::upload_file(object@gene_metadata)
  }

  return(data)
}
