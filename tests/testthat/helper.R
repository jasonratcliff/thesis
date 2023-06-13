suppressPackageStartupMessages({
  library(fs)
  library(withr)
})

build_specimens <- function() {
  specimens <- tibble::tibble(
    Taxon = c(
      paste("Custodis", rep("impavidus", 2)),
      paste("Custodis", rep("'mundus'", 2)),
      paste("Medicari", rep("iugerum", 2)),
      "Medicari profundus ssp. longaevus"
    ),
    Collector = c(rep("A", 4), rep("B", 2), "C"),
    Collection_Number = 1:7,
    Date = as.Date((Sys.Date() - 6):Sys.Date(), origin = "1970-01-01"),
    Herbarium = c("H", "H-1", "H- 1", "H -1", "[H-1], H", "H  ", "H-H"),
    Latitude = seq(from = 41, to = 44, length.out = 7),
    Longitude = seq(from = -107, to = -105, length.out = 7)
  ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        Collection_Number == 1 ~ NA_character_,
        TRUE ~ .data$Taxon
      )
    ) %>%
    Specimen$new(
      records = .,
      identifier = "Taxon"
    )
  return(specimens)
}

build_cartography <- function() {
  specimens <- tibble::tibble(
    State = "Wyoming",
    Species = c("Medicari iugerum", rep("Medicari profundus", 3)),
    Collector = "J. Hooker",
    Collection_Number = 1:4
  ) %>%
    dplyr::bind_cols(
      x = .,
      y = tibble::tibble(
        Latitude = c(45, 44, 44, 45),
        Longitude = c(-110, -110, -109, -109),
      )
    ) %>%
    SpecimenMap$new(
      records = .,
      identifier = "Species"
    )
  return(specimens)
}
