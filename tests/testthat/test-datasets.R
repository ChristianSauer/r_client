context("test-datasets")

BEARER_FROM_ENV = Sys.getenv("BEARERTOKEN")
BASE_URL = Sys.getenv("BASEURL")

test_that("scope is valid", {
  default_conn <- new("FGConnection", base_url = "invalid", bearer_token = "invalid")
  expect_error(get_datasets(default_conn, "bla"), "scope is 'bla' but should be one of: All, Public, Private")
})


test_that("can get datasets successfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  datasets <- get_datasets(default_conn, "All")@content
  n_all_datasets = length(datasets)
  expect_gte(n_all_datasets, 1 )

  datasets <- get_datasets(default_conn, "Private")@content
  n_private_datasets = length(datasets)
  expect_lt(n_private_datasets, n_all_datasets )
})

test_that("can get mass dataset successfully", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  dataset <- get_dataset(default_conn, "mass")
  ds_id <- dataset@Id
  expect_equal(dataset@Id, "mass")
})

test_that("can handle non existing dataset", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(get_dataset(default_conn, "does_not_exist"), "The dataset 'does_not_exist' was not found on the serve")

})

test_that("folder for get data must exist", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(download_dataset(default_conn, "mass", "./imposssible"), "The folder './imposssible' does not exist, please create it")

})

test_that("can download dataset", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  dir.create("./temp")
  result <- download_dataset(default_conn, "mass", "./temp")
  fs <- file.size("./temp/mass.zip")
  expect_gt(fs, 1000)
  file.remove("./temp/mass.zip")
})

test_that("can get valid gene nomenclature", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  data <- get_valid_gene_nomenclatures(default_conn)
  expect_true("Entrez" %in% data)
})

test_that("can get valid matrix formats", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  data <- get_valid_matrix_formats(default_conn)
  expect_true("sparse_cell_gene_expression" %in% data)
})

test_that("can get valid technologies", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  data <- get_valid_technologies(default_conn)
  expect_true("Drop-Seq" %in% data)
})

test_that("can get valid current normalization status", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  data <- get_valid_current_normalization_status(default_conn)
  expect_true("Counts" %in% data)
})


test_that("create: gene nomenclature must be valid", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(create_dataset(default_conn, "title", "description", "short_description", 9606, "matrix_path" , "matrix_format", "gene_nomenclature" ), "The Gene Nomenclature 'gene_nomenclature is unknown. Choose one of: Entrez, GeneSymbol, Ensembl'")

})


test_that("create: organism_id must be valid", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(create_dataset(default_conn, "title", "description", "short_description", "dsgsdfg", "matrix_path" , "matrix_format", "Entrez" ), "The organism id 'dsgsdfg' is not an integer. Choose Homo Sapiens: 9606 Mouse: 10090")

})

test_that("create: matrix format must be valid", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(create_dataset(default_conn, "title", "description", "short_description", 9606, "matrix_path" , "matrix_format", "Entrez" ), "The Matrix format 'matrix_format' is unknown. Choose one of: sparse_cell_gene_expression, sparse_gene_cell_expression, dense_cells_in_rows, dense_cells_in_columns'")

})

test_that("create: matrix path must be valid", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(create_dataset(default_conn, "title", "description", "short_description", 9606, "matrix_path" , "sparse_cell_gene_expression", "Entrez" ), "The file 'matrix_path' does not exist. Please provide a valid file!. See https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/dataset_api.md for valid file formats")

})

test_that("create: works", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  result <- create_dataset(default_conn, "R client test", "description", "short_description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez" )
  expect_is(result, "FGResponse")
})

test_that("FGDatasetUploadParameters: can create", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  data <- FGDatasetUploadParameters()
  expect_is(data, "FGDatasetUploadParameters")
})

test_that("FGDatasetUploadParameters: batch_column cannot be set if no cell metadata", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  expect_error(FGDatasetUploadParameters("FGDatasetUploadParameters", batch_column="something" ), " If batch_column is set, you need to provide a file containing cell_metatadata, too!")

})

test_that("create: optional parameters must be FGDatasetUploadParameters", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  optional <- "BLA"
  expect_error(create_dataset(default_conn, "R client test", "description", "short_description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez",  optional) , "the optional_parameters need to be either NULL or a FGDatasetUploadParameters object")
})

test_that("FGDatasetUploadParameters: validates technology", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  optional <- FGDatasetUploadParameters(technology = "invalid_tech")
  expect_error(create_dataset(default_conn, "R client test", "description", "short_description", 9606, "./matrix.tsv" , "sparse_cell_gene_expression", "Entrez",  optional) , "The Technology 'invalid_tech is unknown. Choose one of")

})

test_that("create: works with optional parameters", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  optional <- FGDatasetUploadParameters(
                                        license ="MIT",
                                        technology = "Smart-Seq",
                                        web_link="https://example.com",
                                        notes="This is a TEST",
                                        citation="FG et al",
                                        batch_column="sample",
                                        current_normalization_status="Counts",
                                        cell_metadata="./cell_metadata.tsv",
                                        gene_metadata="./gene_metadata.tsv"  )
  result <- create_dataset(default_conn,
                           "R client test",
                           "description",
                           "short_description",
                           9606,
                           "./matrix.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez",
                           optional )
  expect_is(result, "FGResponse")
})

test_that("create: shows usefull errors", {
  default_conn <- new("FGConnection", base_url = BASE_URL , bearer_token = BEARER_FROM_ENV)
  optional <- FGDatasetUploadParameters(
    cell_metadata="./test-datasets.R" # <--- should error
  )
  result <- create_dataset(default_conn,
                           "R client test",
                           "description",
                           "short_description",
                           9606,
                           "./matrix.tsv" ,
                           "sparse_cell_gene_expression",
                           "Entrez",
                           optional )
  expect_is(result, "FGErrorResponse")
})
