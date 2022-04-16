# Specimen ID ----

#' Map Individual Specimen
#'
#' Generate geom specifications to add label of individual specimen(s).
#'
#' @param h_adjust Numeric vector of length one for horizontal label adjustment.
#' @param v_adjust Numeric vector of length one for vertical label adjustment.
#' @param label_size Numeric vector of length one for label size.
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @inheritParams find_spp
#' @export
#'
#' @return ggplot object with added specimen annotation layer.
#'
#' @examples
#' ggplot2::ggplot() +
#'   layer_specimens(specimen_tbl = spp_co_front_range,
#'                   id_column = "prior_id", shape_aes = "prior_id") +
#'   spl_id(specimen_tbl = spp_co_front_range,
#'          id_column = "prior_id", shape_aes = "prior_id",
#'          collector = "Wolf", collection = 642)
#'
spl_id <- function(specimen_tbl, id_column, collector, collection,
                   shape_aes = NULL,  geom_size = 3, label_size = 3,
                   h_adjust = 0.25, v_adjust = -0.15) {

  # Call `find_spp()` function to get specimen annotation data.
  spp_id <- Thesis::find_spp(specimen_tbl = specimen_tbl,
                                    collector = collector,
                                    collection = collection) %>%
    dplyr::mutate(
      # Remove collector initials
      taxa_label = stringr::str_remove_all(
        string = .data$Collector,
        pattern = "[A-Z]\\. ?") %>%
        stringr::str_replace(string = ., pattern = "and|with",
                             replacement =  "&") %>%
        paste(., .data$Collection_Number, collapse = "", sep = "\n")
      ) %>%
    # Account for duplicate label matches from joining.
    dplyr::group_by(.data$taxa_label) %>% dplyr::slice(1) %>% dplyr::ungroup()

  # Plot additional map layer to include the specimen returned by spp_find().
  spl_ids <- list(
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, show.legend = FALSE,
      mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude),
      size = 5, shape = 5, fill = NA),
    ggplot2::geom_point(
      data = spp_id, inherit.aes = FALSE, size = geom_size,
      mapping = ggplot2::aes_string(x = "Longitude", y = "Latitude",
                                    colour = id_column, shape = shape_aes)),
    ggplot2::geom_label(
      data = spp_id, inherit.aes = FALSE, alpha = 0.5,
      nudge_x = h_adjust, nudge_y = v_adjust, size = label_size,
      mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude,
                             label = .data$taxa_label),
      label.padding = ggplot2::unit(0.1, "lines"),
     )
  )

  return(spl_ids)
}

