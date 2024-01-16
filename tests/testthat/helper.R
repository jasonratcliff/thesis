suppressPackageStartupMessages({
  library(fs)
  library(withr)
})

build_specimens <- function() {
  specimens <- tibble::tibble(
    scientificName = c(
      paste("Custodis", rep("impavidus", 2)),
      paste("Custodis", rep("'mundus'", 2)),
      paste("Medicari", rep("iugerum", 2)),
      "Medicari profundus ssp. longaevus"
    ),
    recordedBy = c(rep("A", 4), rep("B", 2), "C"),
    recordNumber = 1:7,
    eventDate = as.Date((Sys.Date() - 6):Sys.Date(), origin = "1970-01-01"),
    institutionCode = c("H", "H-1", "H- 1", "H -1", "[H-1], H", "H  ", "H-H"),
    decimalLatitude = seq(from = 41, to = 44, length.out = 7),
    decimalLongitude = seq(from = -107, to = -105, length.out = 7)
  ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        recordNumber == 1 ~ NA_character_,
        TRUE ~ .data$scientificName
      )
    ) %>%
    Specimen$new(
      records = .,
      identifier = "scientificName"
    )
  return(specimens)
}

build_cartography <- function() {
  specimens <- tibble::tibble(
    stateProvince = "Wyoming",
    scientificName = c("Medicari iugerum", rep("Medicari profundus", 3)),
    recordedBy = "J. Hooker",
    recordNumber = 1:4
  ) %>%
    dplyr::bind_cols(
      x = .,
      y = tibble::tibble(
        decimalLatitude = c(45, 44, 44, 45),
        decimalLongitude = c(-110, -110, -109, -109),
      )
    ) %>%
    SpecimenMap$new(
      records = .,
      identifier = "scientificName"
    )
  return(specimens)
}
