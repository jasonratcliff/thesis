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
continuous_tbl <- function(trait_frame, trait_name) {
  min_col <- paste0(trait_name, "_min")
  min_name <- enquo(min_col)
  max_col <- paste0(trait_name, "_max")
  max_name <- enquo(max_col)
  mid_col <- paste0(trait_name, "_mid")
  mid_name <- enquo(mid_col)
  trait_frame %>% dplyr::select(trait_name) %>%
    dplyr::bind_cols(.,
      trait_transform =
        map_dfr(.[[trait_name]], function(x) {
          if (!is.na(x)) {
            trait_split <- gsub(" +", "", x)
            if (grepl("-", x)) {
              trait_split <- strsplit(x, split = "-") %>%
                unlist(.) %>% as.numeric(.)
              trait_split[3] <- (min(trait_split) + max(trait_split)) / 2
              } else {
                trait_split <- rep(as.numeric(x), 3)
                }
            } else {
              trait_split <- rep(NA, 3)
              }
          dplyr::bind_cols(!!min_name := min(trait_split),
                           !!max_name := max(trait_split),
                           !!mid_name := trait_split[3])
          })) %>% dplyr::select(-trait_name) %>% tibble::as_tibble()
}
