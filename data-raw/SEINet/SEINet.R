# Two SEINet searches were performed for "Taxonomic Criteria" of
#   `Scientific Name` with Synonyms included in search.
#
# - Physaria floribunda
# - Physaria bellii
#
# Data were read from occurrence .csv files in Darwin Core format.
# These searches yielded 564 records, 355 with decimal degree coordinates.
#
seinet_records <-
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
                Longitude = "decimalLongitude")

# Filter records with decimal degree coordinate data for .Rda file.
seinet_coords <- seinet_records %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude))

# Distinct records missing coordinate data
seinet_records %>%
  dplyr::filter(is.na(Latitude) | is.na(Longitude)) %>%
  dplyr::n_distinct()

# Table of specimen IDs
seinet_records %>%
  dplyr::group_by(scientificName) %>%
  dplyr::summarise(Count = dplyr::n())

# ggmap plot of SEINet records
ThesisPackage::map_ggmap(specimen_tbl = seinet_coords, size = 7,
                         id_column = "scientificName",
                         gg_map_type = "satellite")

# Write .Rda to data/ subdirectory
usethis::use_data(seinet_coords)
