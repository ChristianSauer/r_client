get_gene_ids = function(spmat){spmat@Dimnames[[1]]}
get_cell_ids = function(spmat){spmat@Dimnames[[2]]}

get_sparse_mat = function(spmat, dir){
    file_name = file.path(dir, "matrix.csv")
    df = data.frame(
        geneId = get_gene_ids(spmat)[spmat@i+1],
        cellId = get_cell_ids(spmat)[spmat@j+1],
        expression = spmat@x)
    message(stringr::str_interp("Saving data in a sparse format as '${file_name}'"))
    write.csv(df, file_name, row.names=FALSE)
    return(file_name)
}

get_sparse_gene_metadata = function(gene_ids, dir){
    file_name = file.path(dir, "gene_metadata.csv")
    df = data.frame(geneId = gene_ids)
    message(stringr::str_interp("Saving gene metadata as '${file_name}'"))
    write.csv(df, file_name, row.names=FALSE)
    return(file_name)
}

get_sparse_cell_metadata = function(cell_metadata, dir){
    file_name = file.path(dir, "cell_metadata.csv")
    message(stringr::str_interp("Saving cell metadata as '${file_name}'"))
    write.csv(cbind(cellId=rownames(cell_metadata), cell_metadata),
              file_name,
              row.names=FALSE)
    return(file_name)
}

get_sparse_files = function(seurat_obj, zipfiles=TRUE){
    data = seurat_obj@data
    gene_ids = get_gene_ids(data)
    cell_metadata = seurat_obj@meta.data
    dir = file.path(tempdir(),
                    stringi::stri_rand_strings(n=1, length = 20)[[1]])
    dir.create(dir)
    files = list(
        matrix_csv = get_sparse_mat(seurat_obj@data, dir),
        gene_metadata = get_sparse_gene_metadata(gene_ids, dir),
        cell_metadata = get_sparse_cell_metadata(cell_metadata, dir))

    if(zipfiles)
        files = lapply(files, zip_file)

    return(files)
}

zip_file = function(file){
    message(stringr::str_interp("compressing file '${file}', this may take a while..."))
    zip_file <- paste(c(basename(file), "zip"), collapse=".")
    zip(zipfile = zip_file, files=file, flags="-j")
    return(zip_file)
}

create_dataset_from_seurat <- function(connection, seurat_obj,
                                       gene_nomenclature, organism_id,
                                       title=seurat_obj@project.name,
                                       zipfiles=TRUE,
                                       description="",
                                       short_description="",
                                       optional_parameters=NULL)
{
    assert_is_connection(connection)
    assert_token_is_not_expired(connection)

    match.arg(gene_nomenclature, c("Entrez", "GeneSymbol", "Ensembl"))

    if(! (organism_id %in% c(9606, 10090)))
        stop(stringr::str_interp("The organism id '${organism_id}' is not an integer. Valid NCBI Ids are integers, e.g. Homo Sapiens: 9606 Mouse: 10090"))


    headers <- get_default_headers(connection)
    headers <- c(headers, httr::progress("up")) # adds a nice progress bar
    url <-  paste(connection@base_url, "dataset/api/v1/datasets", sep="")

    files = get_sparse_files(seurat_obj, zipfiles=zipfiles)

    body = list(
        matrix = httr::upload_file(files[["matrix_csv"]]),
        gene_metadata = httr::upload_file(files[["gene_metadata"]]),
        cell_metadata = httr::upload_file(files[["cell_metadata"]]),
        title = title,
        description = description,
        short_description = short_description,
        organism_id = organism_id,
        matrix_format = "sparse_cell_gene_expression",
        gene_nomenclature = gene_nomenclature)

    optional_data <- list()

    if (!is.null(optional_parameters))
        if (!is(optional_parameters, "FGDatasetUploadParameters"))
            stop("the optional_parameters need to be either NULL or a FGDatasetUploadParameters object. Call new('FGDatasetUploadParameters', ..) to obtain such an object.")
        else
            body <- c(get_data_from_FGDatasetUploadParameters(
                      optional_parameters, connection), body)

    response <- httr::POST(url, headers, body = body)

    if (response["status_code"] == 422) {
                                        # handle errors in the upload, e.g. invalid datatypes
        parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
        validation_errors <- parsed[["validation_errors"]]
        warning(stringr::str_interp("Upload of dataset failed due to these errors: ${validation_errors}"),
                call. = FALSE
                )

        error <- new("FGErrorResponse", path = url, content = parsed, validation_errors=parsed[["validation_errors"]])
        return(error)
    }

    httr::stop_for_status(response) # abort on all other errors

    parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    dataset_id <- parsed[["dataset_id"]]
    result <-new("FGResponse", path = url, content = parsed, DataType="dataset", Id=dataset_id, response=response )
    return(result)
}
