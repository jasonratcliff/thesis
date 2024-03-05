taxa <- readr::read_csv(file = "data-raw/taxa/taxa.csv") |>
  purrr::pmap(
    .f = function(scientificName, scientificNameAuthorship,
                  namePublishedInYear) {
      taxon <- scientificName |>
        strsplit(split = " +") |>
        unlist() |>
        gsub(pattern = "^ +| +$", replacement = "")
      tibble::tibble(
        scientificName,
        scientificNameAuthorship,
        namePublishedInYear,
        genus = taxon[1],
        specificEpithet = taxon[2],
        taxonRank = taxon[3],
        infraspecificEpithet = taxon[4]
      )
    }
  ) |>
    purrr::list_rbind()

codes <- tibble::tribble(
  ~"institutionCode", ~"institutionName",
  "ISTC", "Grant Herbarium (ISTC), University of Northern Iowa"
)

use_data(taxa, codes, internal = TRUE, overwrite = TRUE)
