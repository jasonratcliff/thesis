# Title     : MrBayes *rps* intron `ggtree` gene tree
# Objective : Update repelled tip labels for grouped haplotypes.
# Created by: jasonratcliff
# Created on: 5/25/21

library(thesis)
library(cowplot)
library(dplyr)
library(ggrepel)
library(ggtree)
library(purrr)
library(magrittr)
library(rlang)
library(tibble)

tree <- list()

tree$joined <-
  list.files(
    path = system.file("extdata/MrBayes", package = "thesis"),
    pattern = "rps-infile.nex.con.tre",
    full.names = TRUE
  ) %>%
    thesis::read_tree(tree_file = .) %>%
    thesis::join_bayes(
      tree_data = .,
      id_column = "Taxon_a_posteriori",
      scale_vector = c(5, 10)
    )

# Assign HTML markdown label vector.
tree$labels <-
  c(
    thesis::spl_labels(
      specimen_tbl = tree$joined,
      id_column = "prior_id"
    ),
    thesis::spl_labels(
      specimen_tbl = tree$joined,
      id_column = "Taxon_a_posteriori"
    )
  )

# Assign multi- and single-taxa node labels.
tree$haplotypes <-
  thesis::haplotype_labels(
    haplotypes = tree$joined,
    id_column = "Taxon_a_posteriori"
  )
tree$single <- tree$joined %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(.data$n == 1)

# Subset nodes for labelling posterior probabilities
tree$nodes <- tree$joined %>%
    dplyr::filter(.data$prob != 1)

# Base `ggtree` ----

tree$ggtree <- tree$joined %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::mutate(
    label = dplyr::case_when(
      .data$n > 1 ~ "",
      TRUE ~ .data$label
    )
  ) %>%
  ggtree::ggtree(tr = ., layout = "circular") +
  ggplot2::geom_point(
    data = tree$single, size = 5,
    mapping = ggplot2::aes(
      color = prior_id,
      shape = prior_id
    )
  ) +
  ggplot2::geom_point(
    data = tree$single, size = 3,
    mapping = ggplot2::aes(
      color = Taxon_a_posteriori,
      shape = Taxon_a_posteriori
    )
  ) +
  ggtree::geom_tiplab(hjust = -0.175, size = 3) +
  ggplot2::scale_color_manual(
    values = thesis::spp_color,
    labels = tree$labels
  ) +
  ggplot2::scale_shape_manual(
    values = thesis::spp_shape,
    labels = tree$labels
  ) +
  ggplot2::scale_fill_manual(
    values = thesis::spp_color,
    labels = tree$haplotypes$Label
  ) +
  ggtree::xlim_expand(xlim = 0.01, panel = "Tree") +
  ggtree::geom_treescale(y = 29) +
  ggtree::theme_tree() +
  ggplot2::theme(
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      hjust = 0.5,
      size = 14,
      face = "bold"
    )
  ) +
  ggplot2::labs(color = "Annotations", shape = "Annotations")

# pdf ----

# Separate `ggplot` instance without legend for `cowplot` multi-figure grid.
tree$repel_pdf <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node", ~"Taxon_a_posteriori", ~"color",
    0.025, -1, 0, 5, "Physaria 'medicinae'", "white",
    0.025, -1.5, 0, 5, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.02525, -2, 0.0001, 5, "Physaria eburniflora", "white",
    0.02, -2, 0.001, 5, "Physaria acutifolia", "black",
    0.0305, 0.1, -0.05, 6, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.0275, 0, -0.05, 6, "Physaria didymocarpa subsp. lyrata", "black",
    0.025, -0.25, -0.05, 6, "Physaria acutifolia", "black",
    0.0225, -0.5, -0.05, 6, "Physaria", "black",  # TODO Check reviewed ID
    0.0175, 0, -0.1, 15, "Physaria didymocarpa subsp. lanata", "white",
    0.015, 1, -0.1, 15, "Physaria acutifolia", "black",
    0.0175, 2, -0.01, 18, "Physaria eburniflora", "white",
    0.0225, 2, -0.01, 18, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.0125, 1, -0.01, 19, "Physaria eburniflora", "white",
    0.0075, 0.25, 0, 26, "Physaria vitulifera", "black",
    0.02, -0.625, 0, 4, "Physaria integrifolia", "white",
    0.02, -0.175, 0, 4, "Physaria brassicoides", "white",
    0.021, 0.225, 0, 4, "Physaria acutifolia", "black",
    0.022, 0.6, 0, 4, "Physaria 'medicinae'", "white",
    0.015, -1, 0.1, 8, "Physaria didymocarpa subsp. lanata", "white",
    0.017, -0.75, 0.1, 8, "Physaria brassicoides", "white",
    0.019, -0.5, 0.1, 8, "Physaria acutifolia", "black",
    0.021, -0.25, 0.1, 8, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.0175, 2.35, -0.1, 11, "Physaria dornii", "black",
    0.0165, 1.75, -0.1, 11, "Physaria condensata", "black",
    0.0155, 1.15, -0.1, 11, "Physaria integrifolia", "white"
  ) %>%
    thesis::repel_haplotype_labels(
      tree_nudges = .,
      tree_labels = tree$haplotypes,
      initial_ggtree = tree$ggtree,
      label_size = 2
    ) %>%
    rlang::eval_tidy(expr = .)

# Add node probabilities
tree$repel_pdf <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node",
    -0.005, -2, -0.01, 31,
    -0.0035, 0.75, 0.01, 32,
    0.0025, 2.5, 0.01, 33,
    -0.004, 0, 0.01, 34,
    -0.005, -0.5, 0.01, 35
  ) %>%
    thesis::repel_node_labels(
      node_nudges = .,
      node_labels = tree$nodes,
      initial_ggtree = tree$repel_pdf,
      label_size = 2
    ) %>%
      rlang::eval_tidy(expr = .)

tree$pdf <- tree$repel_pdf +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      override.aes = list(size = 3),
      title.position = "top",
      ncol = 3,
      keyheight = 0.15,
      default.unit = "inch"
    )
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggtext::element_markdown(size = 6),
    plot.margin = ggplot2::margin(
      t = 2,
      r = 1.8,
      b = 1,
      l = 1.75,
      unit = "in"
    )
  )

tree$figure_pdf <-
  cowplot::plot_grid(
    tree$pdf + theme(legend.position = "none"),
    cowplot::get_legend(plot = tree$pdf),
    ncol = 1,
    nrow = 2,
    rel_heights = c(0.75, 0.25)
  )

# png ----

# Separate `ggplot` instance without legend for `cowplot` multi-figure grid.
tree$repel_png <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node", ~"Taxon_a_posteriori", ~"color",
    0.01825, -1.25, 0.1, 5, "Physaria 'medicinae'", "white",
    0.0175, -1.5, 0.1, 5, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.015, -1.5, 0.1, 5, "Physaria eburniflora", "white",
    0.0125, -1.5, 0.1, 5, "Physaria acutifolia", "black",
    0.015, -0.5, -0.01, 6, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.0125, -0.25, -0.01, 6, "Physaria didymocarpa subsp. lyrata", "black",
    0.01, -0.250, -0.01, 6, "Physaria acutifolia", "black",
    0.0075, 0, -0.01, 6, "Physaria", "black",  # TODO Check reviewed ID
    0.01, -0.25, -0.1, 15, "Physaria didymocarpa subsp. lanata", "white",
    0.0075, -0.25, -0.1, 15, "Physaria acutifolia", "black",
    0.0125, 0.75, 0.1, 18, "Physaria eburniflora", "white",
    0.0135, 0.5, 0.1, 18, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.01, 1, -0.1, 19, "Physaria eburniflora", "white",
    0.0075, 0.1, 0, 26, "Physaria vitulifera", "black",
    0.01, -0.66, -0.1, 4, "Physaria integrifolia", "white",
    0.01125, -0.33, -0.1, 4, "Physaria brassicoides", "white",
    0.01175, 0.0425, -0.1, 4, "Physaria acutifolia", "black",
    0.01175, 0.425, -0.1, 4, "Physaria 'medicinae'", "white",
    0.00875, -1.25, 0.1, 8, "Physaria didymocarpa subsp. lanata", "white",
    0.01, -1, 0.1, 8, "Physaria brassicoides", "white",
    0.01125, -0.75, 0.1, 8, "Physaria acutifolia", "black",
    0.0125, -0.5, 0.1, 8, "Physaria didymocarpa subsp. didymocarpa", "white",
    0.0125, 2, -0.1, 11, "Physaria dornii", "black",
    0.0115, 1.5, -0.1, 11, "Physaria condensata", "black",
    0.0105, 1, -0.1, 11, "Physaria integrifolia", "white"
  ) %>%
    thesis::repel_haplotype_labels(
      tree_nudges = .,
      tree_labels = tree$haplotypes,
      initial_ggtree = tree$ggtree,
      label_size = 3
    ) %>%
    rlang::eval_tidy(expr = .)

# Add node probabilities
tree$repel_png <-
  tibble::tribble(
    ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node",
    -0.00375, -1, 0.01, 31,
    -0.0025, 1, -0.01, 32,
    0.0075, 2, -0.05, 33,
    -0.00375, 0.5, 0.01, 34,
    -0.00375, -1, 0.01, 35
  ) %>%
    thesis::repel_node_labels(
      node_nudges = .,
      node_labels = tree$nodes,
      initial_ggtree = tree$repel_png,
      label_size = 4
    ) %>%
      rlang::eval_tidy(expr = .)

tree$png <- tree$repel_png +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      override.aes = list(size = 3),
      title.position = "top",
      ncol = 1,
      keyheight = 0.15,
      default.unit = "inch"
    )
  ) +
  ggplot2::theme(
    legend.position = "right",
    legend.text = ggtext::element_markdown(size = 12),
    plot.margin = ggplot2::margin(
      t = 1.5,
      r = 1.25,
      b = 1,
      l = 1.25,
      unit = "in"
    )
  )

tree$figure_png <-
    cowplot::plot_grid(
      tree$png + theme(legend.position = "none"),
      cowplot::get_legend(plot = tree$png),
      ncol = 2,
      nrow = 1,
      rel_widths = c(0.8, 0.2)
    )

# Write Images ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(tree$figure_png, tree$figure_pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBayes_rps", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
