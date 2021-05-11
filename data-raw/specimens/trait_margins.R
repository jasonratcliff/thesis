library(Thesis)

# Results: ggplot distribution of specimen Basal Leaf Margins.
trait_margins <- Thesis::herbarium_specimens %>%
  separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Basal_leaf_margins"
  ) %>%
  dplyr::filter(
    !grepl(pattern = paste(c("obtuse", "acute"), collapse = "|"),
           x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = capitalize(character_vector = .data$Trait)
  )

usethis::use_data(trait_margins, overwrite = TRUE)
