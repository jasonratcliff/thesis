#' @title Taxonomy
#'
#' @description
#' Constructor class for taxomy-related DWC terms:
#' * `scientificName`
#' * `scientificNameAuthorship`
#' * `genus`
#' * `specificEpithet`
#' * `taxonRank`
#' * `infraspecificEpithet`
#'
#' @details
#' See `data-raw/taxa/taxa.R` for `taxa`, `codes` relational data.
#' * TODO Reconsider use as internal data
#'   - `R/sysdata.rda`
#'
#' @examples
#' taxa <- Taxa$new(taxa = thesis:::taxa)
#' taxa$authorship("Physaria vitulifera")
#' @export
Taxa <- R6::R6Class(
  classname = "Taxa",
  private = list(
    .taxa =  NULL,
    .terms = c(
      "scientificName", "scientificNameAuthorship",
      "genus", "specificEpithet", "taxonRank", "infraspecificEpithet"
    ),
    .set = c("taxon", "authors", "genus", "species", "rank", "infra")
  ),
  public = list(
    #' @description
    #' Full taxonomic names with respective authorships are formatted for
    #' typography (PDF, HTML) with one-to-many relationships against
    #' [thesis::vouchers] term `scientificName`.
    #' @param taxa Taxonomic terms `tbl-df` of names.
    #' @return A `Taxa` object
    initialize = function(taxa) {
      taxa <- taxa[, private$.terms]
      names(taxa) <- private$.set
      private$.taxa <- taxa |>
        purrr::pmap(
          .f = function(...) {
            x <- rlang::dots_list(...)
            authors <- strsplit(x$author, split = " | ", fixed = TRUE) |>
              unlist()
            taxon <- x[c("genus", "species", "infra")]
            taxon <- paste0("_**", taxon[!is.na(taxon)], "**_")
            taxonomy <- c(taxon[1], taxon[2], authors[1],
                          x$rank, taxon[3], authors[2])
            rlang::list2(
              !!x$taxon := paste0(taxonomy[!is.na(taxonomy)], collapse = " ")
            )
          }
        ) |>
        purrr::list_c()
    },
    #' Taxonomic Name with Authorship
    #' @param taxon Lookup value for `scientificName`
    #' @return List of 1 named by `scientificName` with full taxonomic name.
    authorship = function(taxon) {
      private$.taxa[taxon]
    }
  )
)
