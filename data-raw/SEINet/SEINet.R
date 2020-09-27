library(magrittr)

# Two SEINet searches were performed for "Taxonomic Criteria" of
#   `Scientific Name` with Synonyms included in search.
#
# - Physaria floribunda
# - Physaria bellii
#
# Data were read from occurrence .csv files in Darwin Core format.
# These searches yielded 564 records, 355 with decimal degree coordinates.
#
seinet_coords <-
  list.files("data-raw/SEINet", full.names = TRUE) %>%
  purrr::keep(dir.exists(.) == TRUE) %>%
  purrr::map_dfr(function(seinet_dir) {
    list.files(seinet_dir, full.names = TRUE, pattern = "occurrences") %>%
      readr::read_csv(file = .) %>%
      dplyr::select("scientificName", "scientificNameAuthorship", "genus",
                    "specificEpithet", "taxonRank", "infraspecificEpithet",
                    "identifiedBy", "recordedBy", "recordNumber", "eventDate",
                    "year", "stateProvince", "county", "locality",
                    "decimalLatitude", "decimalLongitude",
                    "verbatimCoordinates", "verbatimElevation",
                    "recordId", "references")
    }) %>% dplyr::bind_rows() %>%
  dplyr::rename(Latitude = "decimalLatitude",
                Longitude = "decimalLongitude") %>%

  # Consolidate varietal and subspecific nomenclature.
  dplyr::mutate(
    scientificName = purrr::map_chr(.data$scientificName, function(spp) {
        gsub(pattern = "Physaria floribunda( floribunda)?$",
             replacement = "Physaria floribunda ssp. floribunda", x = spp) %>%
        gsub(pattern = "Physaria floribunda( osterhoutii)?$",
             replacement = "Physaria floribunda var. osterhoutii", x = .) %>%
        gsub(pattern = "subsp\\.|var\\.", replacement = "ssp\\.", x = .)
    })
  )

# Write .Rda to data/ subdirectory
usethis::use_data(seinet_coords, overwrite = TRUE)
