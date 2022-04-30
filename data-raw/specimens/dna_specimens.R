library(thesis)
library(readr)
library(dplyr)
library(purrr)
library(usethis)

# Bind DNA Extraction Information ----

specimens <- list()

# Combine herbarium specimen record information with sequence documentation.
specimens$metadata <-
  readr::read_csv(
    file = "data-raw/specimens/dna_specimens.csv",
    col_types = "ccdcccddcclccc"
  ) %>%
  dplyr::rename(label = taxa_label)

dna_specimens <- purrr::pmap_dfr(
  .l = specimens$metadata,
  .f = function(label, Collector, Collection_Number, ...) {

    # Subset total herbarium record data frame by collector and collection.
    record_match <- thesis::herbarium_specimens %>%
      dplyr::filter(., Collector == !!Collector &
                      Collection_Number == !!Collection_Number)  %>%
      dplyr::mutate(Collection_Number = as.numeric(Collection_Number)) %>%
      dplyr::select(-c(State, County))

    # Join the herbarium records to matching sequencing specimen .csv rows.
    dplyr::left_join(
      x = dplyr::filter(specimens$metadata, label == !!label),
      y = record_match,
      by = c("Collector", "Collection_Number", "Longitude", "Latitude")
    )
  }) %>%

  # Account for duplicate label matches from joining.
  dplyr::group_by(.data$label) %>% dplyr::slice(1) %>% dplyr::ungroup()

usethis::use_data(dna_specimens, overwrite = TRUE)
