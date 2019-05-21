context("test-datasets")

BEARER_PAT <- Sys.getenv("FG_PAT")
BEARER_EMAIL <- Sys.getenv("FG_EMAIL")
BASE_URL <- Sys.getenv("FGBASEURL")
default_conn <- FGConnection$new(BASE_URL, BEARER_PAT, BEARER_EMAIL)

test_that("scope is valid", {
  expect_error(get_datasets(default_conn, "bla"), "scope is 'bla' but should be one of: All, Public, Private")
})


test_that("can get datasets successfully", {
  datasets <- get_datasets(default_conn, "All")@content
  n_all_datasets = length(datasets)
  expect_gte(n_all_datasets, 1 )

  datasets <- get_datasets(default_conn, "Private")@content
  n_private_datasets = length(datasets)
  expect_lt(n_private_datasets, n_all_datasets )
})

test_that("can get mass dataset successfully", {
  dataset <- get_dataset(default_conn, "mass")
  ds_id <- dataset@Id
  expect_equal(dataset@Id, "mass")
})

test_that("can handle non existing dataset", {
  expect_error(get_dataset(default_conn, "does_not_exist"), "The dataset 'does_not_exist' was not found on the serve")

})

test_that("folder for get data must exist", {
  expect_error(download_dataset(default_conn, "mass", "./imposssible"), "The folder './imposssible' does not exist, please create it")
})

test_that("can download dataset", {
  dir.create("./temp")
  result <- download_dataset(default_conn, "mass", "./temp")
  fs <- file.size("./temp/mass.zip")
  expect_gt(fs, 1000)
  file.remove("./temp/mass.zip")
})

test_that("can get valid gene nomenclature", {
  data <- get_valid_gene_nomenclatures(default_conn)
  expect_true("Entrez" %in% data)
})

test_that("can get valid matrix formats", {
  data <- get_valid_matrix_formats(default_conn)
  expect_true("sparse_cell_gene_expression" %in% data)
})

test_that("can get valid technologies", {
  data <- get_valid_technologies(default_conn)
  expect_true("Drop-Seq" %in% data)
})

test_that("create: gene nomenclature must be valid", {
  expect_error(create_dataset(default_conn, "title", "description", 9606, "matrix_path" , "matrix_format", "gene_nomenclature" ), "The Gene Nomenclature 'gene_nomenclature is unknown. Choose one of: Entrez, GeneSymbol, Ensembl'")

})


test_that("create: organism_id must be valid", {
  expect_error(create_dataset(default_conn, "title", "description", "dsgsdfg", "matrix_path" , "matrix_format", "Entrez" ), "The organism id 'dsgsdfg' is not an integer. Valid NCBI Ids are integers, e.g. Homo Sapiens: 9606 Mouse: 10090")
})

test_that("create: matrix format must be valid", {
  expect_error(create_dataset(default_conn, "title", "description", 9606, "matrix_path" , "matrix_format", "Entrez" ), "The Matrix format 'matrix_format' is unknown. Choose one of: sparse_cell_gene_expression, sparse_gene_cell_expression, dense_cells_in_rows, dense_cells_in_columns'")

})

test_that("create: matrix path must be valid", {
  expect_error(create_dataset(default_conn, "@R client test", "description", 9606, "matrix_path" , "sparse_cell_gene_expression", "Entrez" ), "The file 'matrix_path' does not exist. Please provide a valid file!. See https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/dataset_api.md for valid file formats")

})

test_that("create: works", {
  result <- create_dataset(default_conn, "@R client test", "description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez" )
  expect_is(result, "FGResponse")
})

test_that("FGDatasetUploadParameters: can create", {
  data <- FGDatasetUploadParameters()
  expect_is(data, "FGDatasetUploadParameters")
})

test_that("create: optional parameters must be FGDatasetUploadParameters", {
  optional <- "BLA"
  expect_error(create_dataset(default_conn, "@R client test", "description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez",  optional) , "the optional_parameters need to be either NULL or a FGDatasetUploadParameters object")
})

test_that("FGDatasetUploadParameters: validates technology", {
  optional <- FGDatasetUploadParameters(technology = "invalid_tech")
  expect_error(create_dataset(default_conn, "@R client test", "description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez",  optional) , "The Technology 'invalid_tech is unknown. Choose one of")

})

test_that("create: works with optional parameters", {
  optional <- FGDatasetUploadParameters(
                                        license = "MIT",
                                        technology = "Smart-Seq",
                                        web_link = "https://example.com",
                                        citation = "FG et al",
                                        current_normalization_status = "Counts",
                                        cell_metadata = "./cell_metadata.tsv",
                                        gene_metadata = "./gene_metadata.tsv"  )
  result <- create_dataset(default_conn,
                           "@R client test",
                           "description",
                           9606,
                           "./matrix.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez",
                           optional )
  expect_is(result, "FGResponse")
})

test_that("create: shows usefull errors", {
  optional <- FGDatasetUploadParameters(
    cell_metadata = "./test-datasets.R"
  )
  result <- create_dataset(default_conn,
                           "", # <--- should error
                           "description",
                           9606,
                           "./matrix.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez",
                           optional )
  expect_is(result, "FGValidationProblemResponse")
})

test_that("poll: works", {
  result <- create_dataset(default_conn,
                           "@R client test",
                           "description",
                           9606,
                           "./matrix.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez" )

  expect_true(poll_dataset_until_validated(default_conn, result, 1 ))
})

test_that("poll: can cope with failure", {
  result <- create_dataset(default_conn,
                           "@R client test",
                           "description",
                           9606,
                           "./matrixWithError.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez")

  expect_false(poll_dataset_until_validated(default_conn, result, 1 ))
})
