library(ThesisPackage)

#  Assign Specimen Data ----
dna_specimens <- ThesisPackage::dna_specimens %>%
  dplyr::select(-c("Latitude.y", "Longitude.y")) %>%
  dplyr::rename_at(.vars = dplyr::vars(dplyr::matches(".x$")),
                   ~ gsub(pattern = "\\.x$", replacement = "", x = .)) %>%

  # Join coordinate nudges by sample label ID.
  dplyr::left_join(x = .,
                   y = readxl::read_excel("data-raw/mapping/map-labels.xlsx",
    col_types = c("text", "numeric", "numeric"), na = c("", "NA")),
    by = "label") %>%
  dplyr::mutate(x_nudge = ifelse(is.na(x_nudge), 0.25, x_nudge),
                y_nudge= ifelse(is.na(y_nudge), -0.15, y_nudge)) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude))

# Prior Annotations ----
total_priors <- ThesisPackage::herbarium_specimens %>%
  dplyr::select(prior_id, Latitude, Longitude) %>%
  ThesisPackage::subset_coords(specimen_tbl = .,
                               Latitude = c(37, 49.1),
                               Longitude = c(-115.2, -103)) %>%
  # Filter out Lesquerella / Physaria sensu lato spp.
  dplyr::filter(!grepl(paste("Lesquerella", "cnema", "alpina", "cordiformis",
                             "macrantha", sep = "|", collapse = ""),
                       x = prior_id) & !grepl("\\?|Brassicaceae", x = prior_id))

# Build ggplot ----
dna_ggplot <-
  ThesisPackage::map_specimens(specimen_tbl = total_priors, f_adj = 0,
                             id_column = "prior_id", shape_opt = "prior_id") %>%
  ThesisPackage::map_themes(gg_map_obj = ., legend_title = "Prior Annotations",
                            mapped_specimens = total_priors,
                            id_column = "prior_id")

# Construct Call Objects ----
map_labels <-
  purrr::pmap(dna_specimens,
            function(Collector, Collection_Number, x_nudge, y_nudge, ...) {
              rlang::call2(rlang::expr(ThesisPackage::map_spp_id),
                           specimen_tbl = rlang::expr(dna_specimens),
                           gg_map_obj = rlang::expr(.), label_size = 2,
                           id_column = "prior_id",  shape_opt = "prior_id",
                           collector = Collector,
                           collection = Collection_Number,
                           h_adjust = x_nudge, v_adjust = y_nudge)
            }) %>%
  # Reduce the expression to collapse species id label call objects into chain.
  purrr::reduce(~ rlang::expr(!!.x %>% !!.y),
                .init = rlang::expr(dna_ggplot))

# Evaluate call object chain and save plot to file.
dna_ggplot <- rlang::eval_tidy(map_labels)
cowplot::ggsave2(filename = "data-raw/mapping/map-dna.pdf", plot = dna_ggplot,
                 width = 13, height = 13)

print(dna_ggplot)
