# Specimen Mapping Tools ----

#' Filter specimens by coordinate notation and specimen observations.
#'
#' @param map_df Data frame of herbarium specimens to map.
#' @param map_geom Logical vector of length one to subset records with
#' coordinates formatted as decimal degrees.
#' @param map_obs_col Character vector of length one matching column name to
#' subset specimen observations by `map_obs` pattern.
#' @param map_obs Character vector of length one with the observation type by 
#' which to subset specimen records from the `map_obs_col` column.
#' @return Data frame subset of specimen observations.
#'
#' @examples
#' wyo_eburniflora <- subset_spp(taxa_frame = total_physaria,
#'                               state = "Wyoming",
#'                               spp_str = "eburniflora",
#'                               taxa_col = "Taxon")
#'
#' wyo_ebur_remaining <- map_filter(map_df = wyo_eburniflora,
#'                                  map_obs_col = "Physaria_syn",
#'                                  map_obs = "questioned")
#'                                  
#' wyo_ebur_remaining[, c("Physaria_syn", "Collector", "Collection_Number")]
#' #               Physaria_syn Collector Collection_Number
#' # 120 Physaria eburniflora ? J. Haines              4792
#' 
#' wyo_ebur_remaining[, c("Longitude", "Latitude")]
#' #     Longitude Latitude
#' # 120 -107.4294  43.2923
#' 
map_filter <- function(map_df, map_geom = TRUE, map_obs_col = NULL,
                       map_obs = c("confirmed", "questioned", "remaining")) {

  # Check if `map_df` input is a data frame with coordinate vectors.
  if (!is.data.frame(map_df) ||
      (!"Longitude" %in% names(map_df) || !"Latitude" %in% names(map_df))) {
        stop("Input data frame must have longitude / latitude vectors.")
  } else {
    map_subset <- map_df   # Initialize data frame for subset assignment.
  }

  # Filter `map_subset` data frame to rows with degree decimal coordinates.
  if (map_geom == TRUE) {
    map_subset <- subset(map_subset,
                         grepl("^-?[0-9]+\\.?[0-9]+?", map_subset$Longitude) &
                           grepl("[0-9]+\\.?[0-9]+", map_subset$Latitude))
  }

  # Filter specimens by observation type in respective taxa column.
  if (length(map_obs) == 1 && !is.null(map_obs_col)) {
    if(map_obs == "questioned") {
      map_subset <- map_subset[grep("\\?", map_subset[, map_obs_col]), ]
    }
    if(map_obs == "remaining") {
      map_subset <- map_subset[is.na(map_subset[, map_obs_col]), ]
    }
    if(map_obs == "confirmed") {
      map_subset <-
        map_subset[grep("\\?", map_subset[, map_obs_col], invert = TRUE), ]
    }
  } else if (length(map_obs) == 1 && is.null(map_obs_col) ||
             length(map_obs) != 1 && !is.null(map_obs_col)) {
    stop("Enter a single argument for `map_obs` with respective taxa column.")
  }
  
  # Return specimen subset for mapping.
  return(map_subset)
}

