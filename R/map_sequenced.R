# Script to write maps of sequenced specimens distribution.

source("R/map_tools.R")
if (!dir.exists("data/2.distributions/sequenced")) {
  dir.create("data/2.distributions/sequenced")
}
  
# Read in DNA specimen subset assigned in `index.Rmd`.
# .csv file is defined in chunk `sequencedHerbariumRecords`.
dna_map_spp <-
  readr::read_csv("data/1.specimens/dna_map_spp.csv",
                  col_types = readr::cols(
                    label = col_character(),
                    multiLocus = col_character(),
                    Collection_Number = col_double(),
                    Collector.x = col_character(),
                    Collector.y = col_character(),
                    Physaria_a_priori_1 = col_character(),
                    Physaria_a_priori_2 = col_character(),
                    Physaria_a_priori_3 = col_character(),
                    Physaria_a_priori_4 = col_character(),
                    Physaria_recent = col_character(),
                    Physaria_syn = col_character(),
                    Taxon = col_character(),
                    Taxon_a_posteriori = col_character(),
                    ID_prior = col_character(),
                    ID_final = col_logical(),
                    Herbarium.x = col_character(),
                    Herbarium.y = col_character(),
                    State.x = col_character(),
                    State.y = col_character(),
                    County.x = col_character(),
                    County.y = col_character(),
                    Date = col_character(),
                    Date_parsed = col_date(format = ""),
                    Date_md = col_date(format = ""),
                    App.A = col_character(),
                    Latitude = col_double(),
                    Longitude = col_double()
                  )) %>%
  dplyr::filter(multiLocus == TRUE)

# Function wrapper to map sequenced DNA specimens with accession labels.
map_sequenced <- function(dna_map_spp_subs, aes_point,
                          label_subset = "", x_nudge = 0, y_nudge = 0.275,
                          f_adjt = 0.2) {
  map_specimens(map_df = dna_map_spp_subs, map_col = aes_point,
                f_adj = f_adjt) +
    geom_label2(data = dplyr::rename(dna_map_spp_subs,
                                     taxa_label = label),
                mapping = aes(x = Longitude, y = Latitude, label = taxa_label,
                              subset = !(taxa_label %in% label_subset)),
                inherit.aes = FALSE, na.rm = TRUE, label.size = 0.1,
                alpha = 0.5, nudge_x = x_nudge, nudge_y = y_nudge) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    guides(col = guide_legend(ncol = 2))
}

# Set aesthetic
aesthetic <- "Physaria_syn"
# aesthetic <- "Taxon_a_posteriori"
ggplot_legend <- "Priors"
# ggplot_legend <- "Reviewed Identification"

# Western Montana ----

# Label adjustments
seq_mt_w_labels <- c("PDIDY_DI_3794", "PDIDY_LY_3855", "PDIDY_LY_2689")
seq_mt_w_tbl1 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDIDY_DI_3794", "PDIDY_LY_3855"))
seq_mt_w_tbl2 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDIDY_LY_2689"))

# Sequenced western Montana specimens
seq_mt_w_map <-
  dplyr::filter(dna_map_spp,
                Latitude < 50 & Latitude > 44.5 & Longitude < -110.8) %>%
  map_sequenced(dna_map_spp_subs = ., aes_point = aesthetic,
                label_subset = seq_mt_w_labels, y_nudge = 0.275) +
  geom_label2(data = seq_mt_w_tbl1, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in%
                              c("PDIDY_DI_3794", "PDIDY_LY_3855")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.275) +
  geom_label2(data = seq_mt_w_tbl2, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PDIDY_LY_2689")),
              label.size = 0.1, alpha = 0.5, nudge_x = 0.5, nudge_y = 0.15)

# Save western Montana ggplot.
map_themes(seq_mt_w_map, legend_title = ggplot_legend) %>%
  ggsave(plot = ., device = "pdf", scale = 2,
         filename = paste0("data/2.distributions/sequenced/",
                           "map_specimens_mt_west.pdf"))

# Eastern Montana ----

# Label adjustments
seq_mt_e_labels <- c("PDIDY_DI_12677", "PDIDY_LA_32",
                     "PSAXI_SA_74315", "PACUT_48")
seq_mt_e_tbl1 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDIDY_DI_12677", "PDIDY_LA_32"))
seq_mt_e_tbl2 <- dplyr::filter(dna_map_spp,
                               label %in% c("PSAXI_SA_74315"))
seq_mt_e_tbl3 <- dplyr::filter(dna_map_spp,
                               label %in% c("PACUT_48"))

# Sequenced eastern Montana specimens
seq_mt_e_map <-
  dplyr::filter(dna_map_spp,
                Latitude < 50 & Latitude > 44.5 & Longitude > -111) %>%
  map_sequenced(dna_map_spp_subs = ., aes_point = aesthetic,
                label_subset = seq_mt_e_labels, y_nudge = 0.275, f_adjt = 0.2) +
  coord_fixed(xlim = c(-111.4, -103.8), ylim = c(43, 48)) +
  geom_label2(data = seq_mt_e_tbl1, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in%
                              c("PDIDY_DI_12677", "PDIDY_LA_32")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.275) +
  geom_label2(data = seq_mt_e_tbl2, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PSAXI_SA_74315")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.25, nudge_x = -0.5) +
  geom_label2(data = seq_mt_e_tbl3, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PACUT_48")),
              label.size = 0.1, alpha = 0.5, nudge_y = 0.3, nudge_x = -0.2)

# Save eastern Montana ggplot.
map_themes(seq_mt_e_map, legend_title = ggplot_legend) %>%
  ggsave(plot = ., device = "pdf", scale = 2,
         filename = paste0("data/2.distributions/sequenced/",
                           "map_specimens_mt_east.pdf"))

# Western Wyoming ----

# Label adjustments
seq_wy_w_labels <- c("PDORN_4376", "PACUT_7997",
                     "PINTE_17493", "PCOND_16765", "PDORN_42",
                     "PCOND_40", "PCOND_3787", "PINTE_19288",
                     "PDORN_17506", "PDORN_17503",
                     "PCOND_41")
seq_wy_w_tbl1 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDORN_4376", "PACUT_7997"))
seq_wy_w_tbl2 <- dplyr::filter(dna_map_spp,
                               label %in% c("PINTE_17493", "PCOND_16765",
                                            "PDORN_42"))
seq_wy_w_tbl3 <- dplyr::filter(dna_map_spp,
                               label %in% c("PCOND_40", "PCOND_3787",
                                            "PINTE_19288"))
seq_wy_w_tbl4 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDORN_17506", "PDORN_17503"))
seq_wy_w_tbl5 <- dplyr::filter(dna_map_spp,
                               label %in% c("PCOND_41"))

# Sequenced western Wyoming specimens
seq_wy_w_map <-
  dplyr::filter(dna_map_spp,
                Latitude < 45 & Latitude > 40 &
                  Longitude > -112 & Longitude < -108.5) %>%
  map_sequenced(dna_map_spp_subs = ., aes_point = aesthetic,
                label_subset = seq_wy_w_labels, y_nudge = 0.175, f_adjt = 0.7) +
  coord_fixed(xlim = c(-112.5, -108), ylim = c(40.75, 45.2)) +
  geom_label2(data = seq_wy_w_tbl1, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PDORN_4376", "PACUT_7997")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.15) +
  geom_label2(data = seq_wy_w_tbl2, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PINTE_17493", "PCOND_16765",
                                                  "PDORN_42")),
              label.size = 0.1, alpha = 0.5, nudge_x = -0.5) +
  geom_label2(data = seq_wy_w_tbl3, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PCOND_40", "PCOND_3787",
                                                  "PINTE_19288")),
              label.size = 0.1, alpha = 0.5, nudge_x = 0.45, nudge_y = 0.125) +
  geom_label2(data = seq_wy_w_tbl4, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PDORN_17506",
                                                  "PDORN_17503")),
              label.size = 0.1, alpha = 0.5, nudge_x = -0.5, nudge_y = 0.15) +
  geom_label2(data = seq_wy_w_tbl5, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in% c("PCOND_41")),
              label.size = 0.1, alpha = 0.5, nudge_x = 0.3, nudge_y = -0.12)

# Save western Wyoming ggplot.
map_themes(seq_wy_w_map, legend_title = ggplot_legend) %>%
  ggsave(plot = ., device = "pdf",
         filename = paste0("data/2.distributions/sequenced/",
                           "map_specimens_wy_west.pdf"))

# Eastern Wyoming ----

# Label adjustments
seq_wy_e_labels <- c("PDIDY_LA_32", "PDIDY_LA_3138", "PEBUR_10410", "PEBUR_38",
                     "PBRAS_3122", "PBRAS_4225", "PVITU_10105",
                     "PEBUR_14841", "PEBUR_37", "PVITU_16713", "PDIDY_LA_3136")
seq_wy_e_tbl1 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDIDY_LA_32", "PDIDY_LA_3138",
                                            "PEBUR_10410", "PEBUR_38",
                                            "PBRAS_3122", "PBRAS_4225",
                                            "PVITU_10105"))
seq_wy_e_tbl2 <- dplyr::filter(dna_map_spp,
                               label %in% c("PEBUR_14841", "PEBUR_37",
                                            "PVITU_16713"))
seq_wy_e_tbl3 <- dplyr::filter(dna_map_spp,
                               label %in% c("PDIDY_LA_3136"))

# Sequenced eastern Wyoming specimens
seq_wy_east <-
  dplyr::filter(dna_map_spp,
                Latitude < 45 & Latitude > 40 & Longitude > -109) %>%
  map_sequenced(dna_map_spp_subs = ., aes_point = aesthetic,
                label_subset = seq_wy_e_labels, y_nudge = 0.2, f_adjt = 0.25) +
  geom_label2(data = seq_wy_e_tbl1, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in%
                              c("PDIDY_LA_32", "PDIDY_LA_3138", "PEBUR_10410",
                                "PBRAS_3122", "PBRAS_4225", "PVITU_10105",
                                "PEBUR_38")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.2) +
  geom_label2(data = seq_wy_e_tbl2, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in%
                              c("PEBUR_14841", "PEBUR_37", "PVITU_16713")),
              label.size = 0.1, alpha = 0.5, nudge_y = -0.2, nudge_x = -0.5) +
  geom_label2(data = seq_wy_e_tbl3, inherit.aes = FALSE,
              mapping = aes(x = Longitude, y = Latitude, label = label,
                            subset = label %in%
                              c("PDIDY_LA_3136")),
              label.size = 0.1, alpha = 0.5, nudge_x = 0.75)

# Save eastern Wyoming ggplot.
map_themes(seq_wy_east, legend_title = ggplot_legend) %>%
  ggsave(plot = ., device = "pdf",
         filename = paste0("data/2.distributions/sequenced/",
                           "map_specimens_wy_east.pdf"))

# Colorado Utah ----

# Sequenced Colorado / Utah specimens
seq_co_ut <-
  dplyr::filter(dna_map_spp, Latitude < 41 & Latitude > 35) %>%
  map_sequenced(dna_map_spp_subs = ., aes_point = aesthetic)

# Save Colorado / Utah ggplot.
map_themes(seq_co_ut, legend_title = ggplot_legend) %>%
  ggsave(plot = ., device = "pdf", scale = 2,
         filename = paste0("data/2.distributions/sequenced/",
                           "map_specimens_co_ut.pdf"))

