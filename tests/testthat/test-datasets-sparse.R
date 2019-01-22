context("test-datasets-sparse")
library(Matrix)

BEARER_FROM_ENV = Sys.getenv("FGBEARERTOKEN")
BASE_URL = Sys.getenv("FGBASEURL")

gen_data <- function(){
    cell_metadata = data.frame(cellId=c("cell_1", "cell_2", "cell_3"))
    gene_metadata = data.frame(geneId=c("gene_1", "gene_2", "gene_3"))
    matrix = sparseMatrix(
        i=c(1,2), j=c(2,3), x=c(4,5),
        giveCsparse=FALSE, dims=c(3,3), dimnames=list(gene_metadata$geneId, cell_metadata$cellId))
    return(list(cells=cell_metadata, genes=gene_metadata, matrix=matrix))
}

gen_args <- function(replacements=list()){
    default_conn <- new(
        "FGConnection", base_url=BASE_URL, bearer_token=BEARER_FROM_ENV)
    input = gen_data()
    tmpdir = file.path("./temp",
                       stringi::stri_rand_strings(n=1, length = 20)[[1]])
    args = list(connection = default_conn,
                matrix = input$matrix,
                cell_metadata = input$cells,
                gene_metadata = input$genes,
                title = "R client test",
                description = "description",
                short_description = "short_description",
                organism_id = 9606,
                gene_nomenclature = "GeneSymbol",
                tmpdir=tmpdir,
                zipfiles=TRUE)
    args[names(replacements)] = replacements
    args
}

test_that(
    "create-sparse: can create a dataset, unzipped", {
        args <- gen_args(list(zipfiles=FALSE))
        result <- do.call(create_dataset_df, args)
        expect_is(result, "FGResponse")
    })

test_that(
    "create-sparse: can create a dataset, zipped", {
        args <- gen_args(list(zipfiles=TRUE))
        result <- do.call(create_dataset_df, args)
        expect_is(result, "FGResponse")
    })

test_that(
    "create-sparse: can create a dataset, dgCMatrix", {
        args <- gen_args(list(matrix = as(gen_data()$matrix, "dgCMatrix")))
        result <- do.call(create_dataset_df, args)
        expect_is(result, "FGResponse")
    })

test_that(
    "create-sparse: does not change working directory", {
        args <- gen_args()
        result <- do.call(create_dataset_df, args)
        oldwd <- getwd()
        expect_true(getwd()==oldwd)
    })

test_that(
    "create-sparse: temp directory is empty after the submission", {
        args <- gen_args()
        result <- do.call(create_dataset_df, args)
        expect_true(length(list.files(args$tmpdir))==0)
    })

test_that(
    "create-sparse: can poll upload status", {
        args <- gen_args()
        result <- do.call(create_dataset_df, args)
        status <- poll_dataset_until_validated(connection, result)
        expect_true(status)
    })

test_that(
    "create-sparse: wrong matrix type", {
        args <- gen_args(list(matrix = "abc"))
        expect_error(do.call(create_dataset_df, args), "Unsupported matrix format, expected a \"sparseMatrix\".")
    })

test_that(
    "create-sparse: wrong cell_metadata type", {
        args <- gen_args(list(cell_metadata = "abc"))
        expect_error(do.call(create_dataset_df, args), "cell_metadata must be a data frame.")
    })

test_that(
    "create-sparse: wrong gene_metadata type", {
        args <- gen_args(list(gene_metadata = "abc"))
        expect_error(do.call(create_dataset_df, args), "gene_metadata must be a data frame.")
    })

test_that(
    "create-sparse: cell_metadata has no cellId", {
        args <- gen_args(list(cell_metadata = data.frame()))
        expect_error(do.call(create_dataset_df, args), "cell_metadata must have a cellId column.")
    })

test_that(
    "create-sparse: gene_metadata has no geneId", {
        args <- gen_args(list(gene_metadata = data.frame()))
        expect_error(do.call(create_dataset_df, args), "gene_metadata must have a geneId column.")
    })

test_that(
    "create-sparse: no common cell names", {
        args <- gen_args(list(cell_metadata = data.frame(cellId=c(1,2,3))))
        expect_error(do.call(create_dataset_df, args), "No common cell names found in matrix and cell_metadata.")
    })

test_that(
    "create-sparse: no common gene names", {
        args <- gen_args(list(gene_metadata = data.frame(geneId=c(1,2,3))))
        expect_error(do.call(create_dataset_df, args), "No common gene names found in matrix and gene_metadata.")
    })
