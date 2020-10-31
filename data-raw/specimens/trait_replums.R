library(ThesisPackage)

# Results: ggplot distribution of specimen Replum Shape.
trait_replums <-
  separate_discrete_trait(
    specimen_tbl = ThesisPackage::herbarium_specimens,
    trait_selection = "Replum_shape"
  ) %>%
  dplyr::filter(
    !grepl(pattern = paste(c("obtuse", "constricted"), collapse = "|"),
           x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = purrr::map_chr(.x = .data$Trait, function(trait) {
      trait %>%
        gsub("oblanceolate|obovate", "Oblanceolate | Obovate", x = .) %>%
        gsub("elliptic|oblong", "Elliptic | Oblong", x = .) %>%
        capitalize(character_vector = .)
    })
  ) %>% dplyr::distinct()

usethis::use_data(trait_replums, overwrite = TRUE)
