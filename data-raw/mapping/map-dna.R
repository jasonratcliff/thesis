library(thesis)
library(cowplot)
library(dplyr)
library(ggplot2)
library(purrr)
library(readxl)
library(rlang)

#  Assign Specimen Data ----

specimens <- list()

# Join coordinate nudges by sample label ID.
specimens$dna <-
  dplyr::left_join(
    x = thesis::dna_specimens,
    y = readxl::read_excel(
      path = "data-raw/mapping/map-labels.xlsx",
      col_types = c("text", "numeric", "numeric"), na = c("", "NA")),
    by = "label"
  ) %>%
  dplyr::mutate(
    x_nudge = ifelse(is.na(x_nudge), 0.25, x_nudge),
    y_nudge= ifelse(is.na(y_nudge), -0.15, y_nudge)
  ) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude))

# Prior Annotations ----

specimens$prior <- thesis::herbarium_specimens %>%
  dplyr::select(prior_id, Latitude, Longitude) %>%
  thesis::subset_coords(
    specimen_tbl = .,
    Longitude = c(-115.2, -103),
    Latitude = c(37, 49.1)
  ) %>%
  
  # Filter out Lesquerella / Physaria sensu lato spp.
  dplyr::filter(
    !grepl(
      pattern = paste(
        "Lesquerella", "cnema", "alpina", "cordiformis", "macrantha",
        sep = "|", collapse = ""
      ),
      x = .data$prior_id) &
      !grepl("\\?|Brassicaceae", x = .data$prior_id)
  )

# Build ggplot ----

specimens$ggplot <- ggplot2::ggplot() +
  thesis::layer_borders(
    spl_extent = thesis::spl_bbox(specimens$prior),
    sf_county_color = "black") +
  thesis::layer_specimens(
    specimen_tbl = specimens$prior,
    id_column = "prior_id",
    shape_aes = TRUE
  ) +
  thesis::layer_themes(
    specimen_tbl = specimens$prior,
    id_column = "prior_id",
    legend_title = "prior_id"
  ) +
  ggplot2::coord_sf(
    xlim = range(thesis::spl_bbox(specimens$prior)[["Longitude"]]),
    ylim = range(thesis::spl_bbox(specimens$prior)[["Latitude"]])
  ) +
  ggplot2::labs(legend = "Prior Annotations")

# Construct Call Objects ----

specimens$map <-
  purrr::pmap(
    .l = specimens$dna,
    .f = function(Collector, Collection_Number, x_nudge, y_nudge, ...) {
      rlang::call2(
        .fn = rlang::expr(thesis::spl_id),
        specimen_tbl = rlang::expr(specimens$dna),
        collector = Collector,
        collection = Collection_Number,
        id_column = "prior_id",
        shape_aes = "prior_id",
        label_size = 2,
        h_adjust = x_nudge,
        v_adjust = y_nudge
      )
    }) %>%
  
  # Reduce the expression to collapse species id label call objects into chain.
  purrr::reduce(
    .x = .,
    .f = ~ rlang::expr(!!.x + !!.y),
    .init = rlang::expr(specimens$ggplot)
  )

cowplot::save_plot(
  filename = "data-raw/mapping/map-dna.png",
  plot = rlang::eval_tidy(specimens$map),
  base_width = 12,
  base_height = 16
)
