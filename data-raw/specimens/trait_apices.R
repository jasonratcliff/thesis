library(ThesisPackage)

# Results: ggplot distribution of specimen Basal Fruit Apices.
trait_apices <- ThesisPackage::herbarium_specimens %>%
  dplyr::mutate(
    Mature_fruit_apices = purrr::map_chr(
      .x = .data$Mature_fruit_apices,
      function(apex) {
        gsub("-", "_", x = apex)
      })
  ) %>%
  separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Mature_fruit_apices"
  ) %>%
  dplyr::mutate(
    Trait =  purrr::map_chr(.x = .data$Trait, function(apex) {
      split_apex <- strsplit(x = apex, split = "_") %>% unlist()
      ifelse(
        test = length(split_apex) > 1,
        yes = paste(
          c(split_apex[1], sort(split_apex[2:length(split_apex)])),
          collapse = "_"
        ),
        no = split_apex
      )
    }),
    Trait = dplyr::case_when(
      grepl("absent", x = .data$Trait) & grepl("basal", x = .data$Trait)
      ~ "Basal Absent",
      grepl("shallow", x = .data$Trait) & grepl("basal", x = .data$Trait)
      ~ "Basal Shallow",
      grepl(pattern = "[Ee]qual", x = .data$Trait) ~ "Approximately Equal"
    )
  ) %>% dplyr::filter(!is.na(.data$Trait))

usethis::use_data(trait_apices, overwrite = TRUE)
