library(ThesisPackage)

# Results: ggplot distribution of specimen Fruit Trichomes.
trait_trichomes <- ThesisPackage::herbarium_specimens %>%
  separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Fruit_trichomes"
  ) %>%
  dplyr::filter(
    grepl(pattern = paste(c("spreading", "appressed"), collapse = "|"),
          x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = dplyr::case_when(
      grepl("spreading", x = .data$Trait) ~ "Spreading",
      grepl("appressed", x = .data$Trait) ~ "Appressed"
    )
  )

usethis::use_data(trait_trichomes, overwrite = TRUE)
