#' Trait tibble
#'
#' Parse continuous trait range data and return tibble with min, max, and
#' midpoint numeric values with specimen metadata.
#'
#' @param trait_frame Data frame of specimen observations subset from
#'   `total_physaria`.
#' @param trait_name Character vector matching trait column to split.
#' @return Tibble data frame with split trait data from specimen observations.
#'
continuous_trait <- function(trait_frame, trait_name) {
  dplyr::select(trait_frame, dplyr::starts_with("Physaria"),
                Taxon_a_posteriori, Collector, Collection_Number, Date,
                Latitude, Longitude, dplyr::starts_with("Elev"),
                trait_name) %>%
    dplyr::bind_cols(.,
      trait_transform =
        map_dfr(.[[trait_name]], function(x) {
          if (!is.na(x)) {
            trait_split <- gsub(" +", "", x)
            if (grepl("-", x)) {
              trait_split <- strsplit(x, split = "-") %>% 
                unlist(.) %>% as.numeric(.)
              trait_split[3] <- (trait_split[1] + trait_split[2]) / 2
              } else {
                trait_split <- rep(as.numeric(x), 3)
                }
            } else {
              trait_split <- rep(NA, 3)
              }
          dplyr::bind_cols(trait_min = trait_split[1],
                           trait_max = trait_split[2],
                           trait_mid = trait_split[3])
          })) %>% tibble::as_tibble()
}

