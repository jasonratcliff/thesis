# Continuous Traits ----

#' Split Numeric Ranges
#'
#' Given a tibble of range data in a character vector, split ranges separated
#' by "-" character.  Creates two new tibble columns containing numeric vectors
#' with minimum and maximum sorted numeric values split from character range.
#'
#' @param trait_tbl Tibble of specimen observations to split range data from.
#' @param split_var Single column tibble of range data in character vector.
#' @export
#'
#' @return Tibble (3 columns) with character vector of raw ranges and
#'   two numeric vectors of min / max values from ranges split by "-".
#'
#' @examples
#' herbarium_specimens %>%
#'   dplyr::select(Ovule_number) %>%
#'   dplyr::filter(!is.na(Ovule_number)) %>%
#'   range_split(trait_tbl = ., split_var = "Ovule_number")
#' @export
#'
#' @keywords internal
range_split <- function(trait_tbl, split_var) {

  # Count "-" instances and warn when multiple matches are detected.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      stringr::str_count(string = element, pattern = "-")
    }) %>%
    purrr::walk2(.x = ., .y = seq_along(.), function(element, index) {
      ifelse(element > 1,
        warning("Multiple '-' detected at index: ", index), element
      )
    })

  # Split character vector of "-" separated range into list of sorted numerics.
  dplyr::pull(trait_tbl, split_var) %>%
    purrr::map(function(element) {
      if (grepl(pattern = "-", x = element)) {
        stringr::str_split(string = element, pattern = "-") %>%
          unlist() %>%
          as.numeric() %>%
          sort()
      } else {
        as.numeric(c(element, element))
      }
    }) %>%
    # Cast list of split range data into tibble and add column of original data.
    plyr::ldply(fun = rbind) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::mutate(!!split_var := dplyr::pull(trait_tbl, split_var)) %>%
    dplyr::select(1:2) %>%
    # Rename tibble variables wth minimum and maximum range data.
    # https://dplyr.tidyverse.org/articles/programming.html#different-input-and-output-variable
    dplyr::rename(
      !!paste0(split_var, "_min") := "V1",
      !!paste0(split_var, "_max") := "V2"
    )
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
#' @keywords internal
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
        }
      )
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
#' @return A `ggplot` with specimen trait distribution mapping.
#'
#' @examples
#' thesis::herbarium_specimens %>%
#'   dplyr::filter(!is.na(.data$Ovule_number)) %>%
#'   dplyr::bind_cols(
#'     .,
#'     range_split(trait_tbl = ., split_var = "Ovule_number")
#'   ) %>%
#'   dplyr::rename(Trait = "Ovule_number_max") %>%
#'   dplyr::mutate(Trait = as.factor(x = .data$Trait)) %>%
#'   map_trait_distribution(tidy_trait = .) +
#'   ggplot2::scale_color_viridis_d(option = "D") +
#'   ggplot2::labs(color = "Max Ovule Count per Locule")
#'
#' @keywords internal
map_trait_distribution <- function(tidy_trait,
                                   bb_xlim = c(-114, -102.5),
                                   bb_ylim = c(37.5, 48.75)) {
  trait_map <- ggplot2::ggplot(data = tidy_trait) +
    layer_borders(
      spl_extent = spl_bbox(tidy_trait),
      sf_county_color = "black"
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(
        x = .data$Longitude, y = .data$Latitude,
        color = .data$Trait
      ),
      na.rm = TRUE
    ) +
    ggplot2::coord_sf(xlim = bb_xlim, ylim = bb_ylim) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
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
#' @param trait Variable with trait observation in
#'   [thesis::herbarium_specimens].
#' @param aesthetic_id Variable to set color and shape aesthetics of jitters.
#' @param aesthetic_labels Named vector of labels for manual scale values.
#' @param violin.params List of arguments passed to [ggplot2::geom_violin().
#' @param jitter.params List of arguments passed to [ggplot2::geom_jitter()].
#' @param theme.params List of arguments passed to [ggplot2::theme()].
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @export
#'
#' @return [ggplot] of trait observations delineated by reviewed id (x-axis)
#'   and jittered points (y-axis) representing numeric values.
#'
#' @keywords internal
jitter_violin <- function(specimen_tbl, trait, aesthetic_id,
                          aesthetic_labels = NULL, legend_title = NULL,
                          violin.params = list(),
                          jitter.params = list(),
                          theme.params = list()) {
  # Text markdown label replacements to legend text from prior & reviewed IDs.
  if (is.null(aesthetic_labels)) {
    aesthetic_labels <-
      thesis::spl_labels(
        specimen_tbl = specimen_tbl,
        id_column = aesthetic_id
      )
  }

  violins <- do.call("geom_violin", utils::modifyList(
    list(
      mapping = aes_(x = quote(Taxon_a_posteriori), y = as.name(trait)),
      na.rm = TRUE
    ),
    val = violin.params
  ))

  jitters <- do.call("geom_jitter", utils::modifyList(
    list(
      mapping = aes_(
        x = quote(Taxon_a_posteriori),
        y = as.name(trait),
        color = as.name(aesthetic_id),
        shape = as.name(aesthetic_id)
      ),
      na.rm = TRUE
    ),
    val = jitter.params
  ))

  themes <- do.call("theme", utils::modifyList(
    list(
      title = ggtext::element_markdown(),
      axis.title.x = ggplot2::element_blank(),
      legend.position = "none"
    ),
    val = theme.params
  ))

  ggplot(data = specimen_tbl) +
    ggplot2::theme_classic() +
    violins +
    jitters +
    ggplot2::scale_color_manual(
      name = legend_title, labels = aesthetic_labels,
      values = thesis::spp_color, na.value = "black"
    ) +
    ggplot2::scale_shape_manual(
      name = legend_title, labels = aesthetic_labels,
      values = thesis::spp_shape, na.value = 17
    ) +
    themes
}

#' Plot Trait Phenology
#'
#' Wrapper to [ggplot()] layering points by collection date for a given
#' y variable speecified by `trait`. The `aesthetic_id` determines
#' **color** and **shape** geom point aesthetics and `aesthetic_labels`
#' are passed as manual scale specifications. Use `legend_title` to set
#' the combined color and shape scale legend title.
#'
#' Additional paramaters can be passed to `geom_jitter()` via `jitter.params`
#' and [theme()] via `theme.params()`, where each `\*.params()` argument
#' takes a list
#'
#' @inheritParams jitter_violin
#' @inheritParams layer_themes
#' @export
#'
#' @return [ggplot] object of trait phenology by "%m-%d" date format.
#'
#' @examples
#' herbarium_specimens %>%
#' \dontrun{
#'   subset_coords(Longitude = c(-108, -105), Latitude = c(39, 42)) %>%
#'   dplyr::bind_cols(
#'     .,
#'     range_split(trait_tbl = ., split_var = "Mature_fruit_length_mm")
#'   ) %>%
#'   trait_phenology(
#'     specimen_tbl = .,
#'     trait = "Mature_fruit_length_mm_max",
#'     aesthetic_id = "Taxon_a_posteriori",
#'     legend_title = "Reviewed Annotations"
#'   )
#' }
#' @keywords internal
trait_phenology <- function(specimen_tbl, trait, aesthetic_id,
                            aesthetic_labels = NULL, legend_title = NULL,
                            jitter.params = list(),
                            theme.params = list()) {
  specimen_tbl <- dplyr::filter(
    specimen_tbl,
    !is.na(.data$Date_md) & !is.na(.data[[trait]])
  )

  # Text markdown label replacements to legend text from prior & reviewed IDs.
  if (is.null(aesthetic_labels)) {
    aesthetic_labels <-
      thesis::spl_labels(
        specimen_tbl = specimen_tbl,
        id_column = aesthetic_id
      )
  }

  jitters <- do.call("geom_point", utils::modifyList(
    list(na.rm = TRUE),
    val = jitter.params
  ))

  scales <- list(
    ggplot2::scale_x_date(
      name = "Collection Date",
      date_labels = "%b",
      limits = as.Date(c("05-01", "08-31"), format = "%m-%d")
    ),
    ggplot2::scale_color_manual(
      name = legend_title,
      labels = aesthetic_labels,
      values = thesis::spp_color,
      na.value = "black"
    ),
    ggplot2::scale_shape_manual(
      name = legend_title,
      labels = aesthetic_labels,
      values = thesis::spp_shape,
      na.value = 17
    )
  )

  themes <- do.call("theme", utils::modifyList(
    list(legend.text = ggtext::element_markdown()),
    val = theme.params
  ))

  ggplot2::ggplot(
    data = specimen_tbl,
    mapping = aes_(
      x = quote(Date_md),
      y = as.name(trait),
      color = as.name(aesthetic_id),
      shape = as.name(aesthetic_id)
    )
  ) +
    ggplot2::geom_smooth(method = "lm", na.rm = TRUE, formula = y ~ x) +
    ggplot2::theme_classic() +
    jitters +
    scales +
    themes
}

#' Extract Legend of [ggplot] Aesthetics from Specimen Annotations
#'
#' @param ncol Number of legend columns.
#' @inheritParams jitter_violin
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @export
#'
#' @return A `grob` with extracted plot legend of annotation aesthetics.
#'
#' @examples
#' specimen_tbl <- herbarium_specimens %>%
#' \dontrun{
#'   subset_coords(Longitude = c(-108, -105), Latitude = c(39, 42))
#' aesthetic_id <- "Taxon_a_posteriori"
#' legend_title <- "Reviewed Annotations"
#' aesthetic_labels <- NULL
#' theme.params <- list()
#' guide.params <- list()
#' }
#' @keywords internal
annotation_legend <- function(specimen_tbl, aesthetic_id, ncol = 2,
                              legend_title, aesthetic_labels = NULL,
                              theme.params = list()) {
  # Text markdown label replacements to legend text from prior & reviewed IDs.
  if (is.null(aesthetic_labels)) {
    aesthetic_labels <-
      thesis::spl_labels(
        specimen_tbl = specimen_tbl,
        id_column = aesthetic_id
      )
  }

  plot_themes <- do.call("theme", utils::modifyList(
    list(
      title = ggtext::element_markdown(),
      axis.text = ggplot2::element_text(angle = 45, hjust = 1),
      legend.title = ggplot2::element_text(colour = "grey"),
      legend.title.align = 0.5,
      legend.justification = "center",
      legend.position = "bottom",
      legend.text = ggtext::element_markdown()
    ),
    val = theme.params
  ))

  plot_layers <-
    list(
      geom_jitter(
        aes_(
          x = quote(Taxon_a_posteriori),
          y = as.name(aesthetic_id),
          color = as.name(aesthetic_id),
          shape = as.name(aesthetic_id)
        ),
        width = 0.25, height = 0.25
      ),
      ggplot2::scale_color_manual(
        name = legend_title,
        labels = aesthetic_labels,
        values = thesis::spp_color,
        na.value = "black"
      ),
      ggplot2::scale_shape_manual(
        name = legend_title,
        labels = aesthetic_labels,
        values = thesis::spp_shape,
        na.value = 17
      ),
      ggplot2::guides(
        "color" = ggplot2::guide_legend(
          ncol = ncol,
          title.position = "top"
        ),
        "shape" = ggplot2::guide_legend(
          ncol = ncol,
          title.position = "top"
        )
      ),
      ggplot2::theme_classic(),
      ggplot2::labs(x = "Reviewed", y = "Prior")
    )

  built_ggplot <- ggplot(data = specimen_tbl) +
    plot_layers +
    plot_themes

  ggplot_legend <- cowplot::get_legend(built_ggplot)
  return(ggplot_legend)
}
