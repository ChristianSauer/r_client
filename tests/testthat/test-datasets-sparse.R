context("test-datasets-sparse")

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

test_that(
    "create-sparse: can create a dataset", {
        default_conn <- new(
            "FGConnection", base_url=BASE_URL, bearer_token=BEARER_FROM_ENV)
        input <- gen_data()
        result <- create_dataset_df(default_conn,
                                    matrix = input$matrix,
                                    cell_metadata = input$cells,
                                    gene_metadata = input$genes,
                                    title = "R client test",
                                    description = "description",
                                    short_description = "short_description",
                                    organism_id = 9606,
                                    gene_nomenclature = "GeneSymbol",
                                    tmpdir="./tmpdata")
        expect_is(result, "FGResponse")
    })
