library(Thesis)

# Results: ggplot distribution of specimen Ovule Number (per locule).
trait_ovules <- Thesis::herbarium_specimens %>%
  dplyr::select(
    "prior_id", "Taxon_a_posteriori", "Ovule_number",
    "Latitude", "Longitude", "Collector", "Collection_Number"
  ) %>%
  dplyr::filter(!is.na(.data$Ovule_number)) %>%
  dplyr::bind_cols(., range_split(trait_tbl = .,
                                  split_var = "Ovule_number")) %>%
  dplyr::rename(Trait = "Ovule_number_max") %>%
  dplyr::mutate(
    Trait = as.factor(x = .data$Trait)
  )

usethis::use_data(trait_ovules, overwrite = TRUE)
