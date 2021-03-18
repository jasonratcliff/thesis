# Continuous Traits ----

#' Split Numeric Ranges
#'
#' Given a tibble of range data in a character vector, split ranges separated
#' by "-" character.  Creates two new tibble columns containing numeric vectors
#' with minimum and maximum sorted numeric values split from character range.
#'
#' @param trait_tbl Tibble of specimen observations to split range data from.
#' @param split_var Single column tibble of range data in character vector.
#' @importFrom rlang .data :=
#' @export
#'
#' @return Tibble (3 columns) with character vector of raw ranges and
#'   two numeric vectors of min / max values from ranges split by "-".
#'
#' @examples
#' herbarium_specimens %>%
#'   dplyr::select(Ovule_number) %>% dplyr::filter(!is.na(Ovule_number)) %>%
#'    range_split(trait_tbl = ., split_var = "Ovule_number")
#' @export
#'
range_split <- function(trait_tbl, split_var) {

  # Count "-" instances and warn when multiple matches are detected.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      stringr::str_count(string = element, pattern = "-")
    }) %>%
    purrr::walk2(.x = ., .y = seq_along(.), function(element, index) {
      ifelse(element > 1,
             warning("Multiple '-' detected at index: ", index),  element)
    })

  # Split character vector of "-" separated range into list of sorted numerics.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      if (grepl(pattern = "-", x = element)) {
        stringr::str_split(string = element, pattern = "-") %>%
          unlist() %>% as.numeric() %>% sort()
      } else { as.numeric(c(element, element)) }
    }) %>%

    # Cast list of split range data into tibble and add column of original data.
    plyr::ldply(fun = rbind) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::mutate(!!split_var := dplyr::pull(trait_tbl, split_var)) %>%
    dplyr::select(1:2) %>%

    # Rename tibble variables wth minimum and maximum range data.
    # https://dplyr.tidyverse.org/articles/programming.html#different-input-and-output-variable
    dplyr::rename(!!paste0(split_var, "_min") := "V1",
                  !!paste0(split_var, "_max") := "V2")

}

# Discrete Traits ----

#' Separate Discrete Trait Observations
#'
#' Separate records of discrete trait observations from a variable where
#' multiple values may be observed in a single voucher.
#'
#' @param trait_selection Tibble variable to split discrete observations from.
#' @param filter_n Cutoff for filtering rarely observed traits by count.
#' @inheritParams layer_specimens
#' @export
#'
#' @return Tibble of specimen observations separated into rows by unique trait
#'   observations.
#'
#' @examples
#' separate_discrete_trait(
#'   specimen_tbl = herbarium_specimens,
#'   trait_selection = "Replum_shape"
#' )
#'
separate_discrete_trait <- function(specimen_tbl, trait_selection,
                                    filter_n = 20) {
  tidy_trait <- specimen_tbl %>%
    dplyr::select(
      "prior_id", "Taxon_a_posteriori",
      "Latitude", "Longitude", "Collector", "Collection_Number",
      !!trait_selection
    ) %>%
    dplyr::filter(!is.na(.data[[!!trait_selection]])) %>%
    dplyr::mutate(
      Trait = purrr::map_chr(
        .x = .data[[!!trait_selection]], function(observation) {
          stringr::str_remove_all(
            string = observation,
            pattern = "[\\(\\)]"
          ) %>%
            stringr::str_to_lower(string = .)
        })
    ) %>%
    tidyr::separate_rows(data = ., .data$Trait, sep = "[,; -]") %>%
    dplyr::add_count(.data$Trait) %>%
    dplyr::filter(nchar(.data$Trait) > 0, .data$n > filter_n) %>%
    dplyr::select(-c(!!trait_selection))
  return(tidy_trait)
}

# Trait Mapping ----

#' Trait Distribution Map
#'
#' Given a trait subset (e.g., discrete or continuous morphological characters),
#' build a `ggplot` over state and county border layers returned by
#' [layer_borders()] and with standardized theme and coordinates.
#'
#' @param tidy_trait Tibble of specimen trait distributions.
#' @param bb_xlim Numeric vctor to scale `sf` coordinates x-limits.
#' @param bb_ylim Numeric vctor to scale `sf` coordinates y-limits.
#' @export
#'
#' @return A `ggplot` with specimen trait distributin mapping.
#'
#' @examples
#' map_trait_distribution(tidy_trait = trait_ovules) +
#'   ggplot2::scale_color_viridis_d(option = "D") +
#'   ggplot2::labs(color = "Max Ovule Count per Locule")
#'
map_trait_distribution <- function(tidy_trait,
                                   bb_xlim = c(-114, -102.5),
                                   bb_ylim = c(37.5, 48.75)) {
  trait_map <- ggplot2::ggplot(data = tidy_trait) +
    layer_borders(spl_extent = spl_bbox(tidy_trait),
                  sf_county_color = "black") +
    ggplot2::geom_jitter(
      ggplot2::aes(x = .data$Longitude, y = .data$Latitude,
                   color = .data$Trait),
      na.rm = TRUE
    ) +
    ggplot2::coord_sf(xlim = bb_xlim, ylim = bb_ylim) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color =  "black"),
      legend.key = ggplot2::element_blank()
    )
  return(trait_map)
}

# `ggplot` Wrappers ----


#' Built violin plots with jittered points
#'
#' Semi-flexible plotting of continuous trait violin plots with adjustable
#' point jitters.
#'
#' @inheritParams layer_specimens
#' @param trait Variable with trait observation in
#'   [ThesisPackage::herbarium_specimens].
#' @param violin.params List of arguments passed to [ggplot2::geom_violin().
#' @param jitter.paramsList List of arguments passed to [ggplot2::geom_jitter()].
#' @param theme.params List of arguments passed to [ggplot2::theme()].
#' @export
#'
#' @return [ggplot] of trait observations delineated by reviewed id (x-axis)
#'   and jittered points (y-axis) representing numeric values.
#'
jitter_violin <- function(specimen_tbl, trait,
                         violin.params = list(),
                         jitter.params = list(),
                         theme.params = list()) {

  violins <- do.call("geom_violin", modifyList(
    list(mapping = aes_(x = quote(Taxon_a_posteriori), y = as.name(trait)),
         na.rm = TRUE),
    val = violin.params)
  )
  jitters <- do.call("geom_jitter", modifyList(
    list(mapping = aes_(x = quote(Taxon_a_posteriori), y = as.name(trait),
                        color = quote(prior_id), shape = quote(prior_id)),
         na.rm = TRUE),
    val = jitter.params)
  )
  themes <- do.call("theme", modifyList(
    list(title = ggtext::element_markdown(),
         axis.title.x = element_blank(),
         legend.position = "none"),
    val = theme.params)
  )

  ggplot(data = specimen_tbl) +
    scale_color_manual(values = ThesisPackage::spp_color, na.value = "black") +
    scale_shape_manual(values = ThesisPackage::spp_shape, na.value = 17) +
    theme_classic() +
    violins +
    jitters +
    themes
}



