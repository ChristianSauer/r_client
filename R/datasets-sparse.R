library(Matrix)
library(zip)

get_gene_ids = function(spmat){spmat@Dimnames[[1]]}
get_cell_ids = function(spmat){spmat@Dimnames[[2]]}

matrix_to_file = function(spmat, dir){
    spmat = as(spmat, "dgTMatrix")
    file_name = file.path(dir, "matrix.csv")
    df = data.frame(
        cellId = get_cell_ids(spmat)[spmat@j+1],
        geneId = get_gene_ids(spmat)[spmat@i+1],
        expression = spmat@x)
    message(stringr::str_interp("Saving expression matrix in a sparse format as '${file_name}'"))
    write.csv(df, file_name, row.names=FALSE)
    return(file_name)
}

gene_metadata_to_file = function(gene_metadata, dir){
    file_name = file.path(dir, "gene_metadata.csv")
    message(stringr::str_interp("Saving gene metadata as '${file_name}'"))
    write.csv(gene_metadata, file_name, row.names=FALSE)
    return(file_name)
}

cell_metadata_to_file = function(cell_metadata, dir){
    file_name = file.path(dir, "cell_metadata.csv")
    message(stringr::str_interp("Saving cell metadata as '${file_name}'"))
    write.csv(cell_metadata, file_name, row.names=FALSE)
    return(file_name)
}

create_tmp_files = function(matrix, cell_metadata, gene_metadata, tmpdir=NULL){
    if(is.null(tmpdir)){
        tmpdir = file.path(tempdir(),
                           stringi::stri_rand_strings(n=1, length = 20)[[1]])
    }
    dir.create(tmpdir)
    tmpdir = normalizePath(tmpdir)

    files = list(
        matrix_csv = matrix_to_file(matrix, tmpdir),
        gene_metadata = gene_metadata_to_file(gene_metadata, tmpdir),
        cell_metadata = cell_metadata_to_file(cell_metadata, tmpdir))

    return(files)
}

zip_file = function(file){
    message(stringr::str_interp("compressing file '${file}', this may take a while..."))
    zip_file <- paste(c(file, "zip"), collapse=".")
    oldwd = getwd()

    tryCatch({
        setwd(dirname(file))
        zip(zip_file, basename(file))
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

#' Submits a data set from a sparse matrix of dgTmatrix type and
#' gene/cell metadata.  dgTMatrix has to have named dimensions, cell
#' and gene metadata must have cellId and geneId columns respectively.
#' The names of the dimensions in the matrix must be a subset of the
#' respective gene/cell metadata data frames.
#'
#' @param connection FASTGenomics connection object
#' @param matrix sparseMatrix storing the expression table.  The first
#'     dimension is assumed to be genes and the second must be cell
#'     names.
#' @param cell_metadata dataframe with cell metadata, must have a
#'     cellId column.
#' @param gene_metadata dataframe with gene metadata, must have a
#'     geneId column.
#' @param organism_id One of 9606 (Mouse) and 10090 (Human)
#' @param title The title of the data set
#' @param short_description short description, Default: ""
#' @param description long description, Default: ""
#' @param zipfiles Weather to compress files before submitting,
#'     Default: TRUE
#' @param tmpdir The location of temporary files, Default: NULL
#' @param optional_parameters Object representing further optional
#'     parameters, see \code{\link{FGDatasetUploadParameters}},
#'     Default: NULL
#'
#' @return either of \code{\link{FGResponse}},
#'     \code{\link{FGErrorResponse}},
#'     \code{\link{FGErrorModelResponse}},
#'     \code{\link{FGValidationProblem}}.
#'
#' @export
create_dataset_df <- function(connection, matrix, cell_metadata,
                              gene_metadata, gene_nomenclature,
                              organism_id, title, zipfiles=TRUE,
                              description="", short_description="",
                              optional_parameters=NULL, tmpdir=NULL)
{
    assert_is_connection(connection)
    assert_token_is_not_expired(connection)

    match.arg(gene_nomenclature, c("Entrez", "GeneSymbol", "Ensembl"))

    if(! (organism_id %in% c(9606, 10090)))
        stop(stringr::str_interp("The organism id '${organism_id}' is not an integer. Valid NCBI Ids are integers, e.g. Homo Sapiens: 9606 Mouse: 10090"))

    ## check if the matrix is sparse
    if( !is(matrix, "sparseMatrix") ){
        stop("Unsupported matrix format, expected a \"sparseMatrix\".")
    }
    if( !is(cell_metadata, "data.frame")){
        stop("cell_metadata must be a data frame.")
    }
    if( !is(gene_metadata, "data.frame")){
        stop("gene_metadata must be a data frame.")
    }
    if( ! 'cellId' %in% colnames(cell_metadata) ){
        stop("cell_metadata must have a cellId column.")
    }
    if( ! 'geneId' %in% colnames(gene_metadata) ){
        stop("gene_metadata must have a geneId column.")
    }
    if( length(intersect(get_cell_ids(matrix), cell_metadata$cellId)) == 0 ){
        stop("No common cell names found in matrix and cell_metadata.")
    }
    if( length(intersect(get_gene_ids(matrix), gene_metadata$geneId)) == 0 ){
        stop("No common gene names found in matrix and gene_metadata.")
    }

    # adds a nice progress bar
    headers <- c(get_default_headers(connection), httr::progress("up"))
    url <- paste(connection@base_url, "dataset/api/v1/datasets", sep="")

    files = create_tmp_files(matrix, cell_metadata, gene_metadata, tmpdir=tmpdir)
    if(zipfiles)
        files = lapply(files, zip_file)

    body = list(
        matrix = httr::upload_file(files[["matrix_csv"]]),
        title = title,
        description = description,
        short_description = short_description,
        organism_id = organism_id,
        matrix_format = "sparse_cell_gene_expression",
        gene_nomenclature = gene_nomenclature)

    optional_data <- list()

    tryCatch({
        if (!is.null(optional_parameters))
            if (!is(optional_parameters, "FGDatasetUploadParameters"))
                stop("the optional_parameters need to be either NULL or a FGDatasetUploadParameters object. Call new('FGDatasetUploadParameters', ..) to obtain such an object.")
            else
            {
                if(optional_parameters@gene_metadata != "")
                    message("Warning, replacing gene_metadata with a table inferred from the Seurat object")

                if(optional_parameters@cell_metadata != "")
                    message("Warning, replacing cell_metadata with a table inferred from the Seurat object")

                optional_parameters@gene_metadata = files[["gene_metadata"]]
                optional_parameters@cell_metadata = files[["cell_metadata"]]

                body <- c(get_data_from_FGDatasetUploadParameters(
                    optional_parameters, connection), body)
            }

        response <- httr::POST(url, headers, body = body)},
        error = stop,
        finally = {lapply(files, file.remove)}
        )
    return(parse_response(response, "dataset"))
}

#' Uploads a seurat dataset.  Runs \code{\link{create_dataset_df}}
#' with matrix, cell_metadata and gene_metadata inferred from the
#' seurat object.
#'
#' @param connection FASTGenomics connection object
#' @param seurat_obj Seurat object to submitted
#' @param ... Other parameters passed to \code{\link{create_dataset_df}}
#'
#' @export
create_dataset_from_seurat <- function(connection, seurat_obj, ...){
    matrix = as(seurat_obj@data, "dgTMatrix")
    cell_metadata = seurat_obj@meta.data
    cell_metadata = cbind(cellId=rownames(cell_metadata), cell_metadata)
    gene_metadata = data.frame(geneId = seurat_obj@data@Dimnames[[1]])

    create_dataset_df(connection,
                      matrix = matrix,
                      cell_metadata = cell_metadata,
                      gene_metadata = gene_metadata,
                      title = seurat_obj@project.name,
                      ...)
}
