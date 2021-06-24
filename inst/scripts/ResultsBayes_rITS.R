# Title     : MrBayes rITS `ggtree` Gene Tree
# Objective : Update repelled tip labels for grouped haplotypes.
# Created by: jasonratcliff
# Created on: 5/19/21

library(Thesis)
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
    path = system.file("extdata/MrBayes", package = "Thesis"),
    pattern = "rITS-infile.nex.con.tre",
    full.names = TRUE
  ) %>%
    Thesis::read_tree(tree_file = .) %>%
    Thesis::join_bayes(
      tree_data = .,
      id_column = "Taxon_a_posteriori",
      scale_vector = c(5, 10)
    )

# Assign HTML markdown label vector.
tree$labels <-
  c(
    Thesis::spl_labels(
      specimen_tbl = tree$joined,
      id_column = "prior_id"
    ),
    Thesis::spl_labels(
      specimen_tbl = tree$joined,
      id_column = "Taxon_a_posteriori"
    )
  )

# Assign multi- and single-taxa node labels.
tree$haplotypes <-
  Thesis::haplotype_labels(
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
    values = Thesis::spp_color,
    labels = tree$labels
  ) +
  ggplot2::scale_shape_manual(
    values = Thesis::spp_shape,
    labels = tree$labels
  ) +
  ggplot2::scale_fill_manual(
    values = Thesis::spp_color,
    labels = tree$haplotypes$Label
  ) +
  ggtree::xlim_expand(xlim = 0.01, panel = "Tree") +
  ggtree::geom_treescale(y = 44) +
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
    0.0175, -0.5, -0.1, 3, "Physaria vitulifera", "black",
    0.015, -1, -0.1, 3, "Physaria acutifolia", "black",
    0.02, -2.25, 0.1, 8, "Physaria acutifolia", "black",
    0.0225, -2, 0.1, 8, "Physaria condensata", "black",
    0.02425, -1.6, 0.1, 8, "Physaria dornii", "black",
    0.027, -1.5, 0.1, 8, "Physaria integrifolia", "white",
    0.029, -1.25, 0.1, 8, "Physaria didymocarpa subsp. lanata", "white",
    0.031, -1, 0.1, 8, "Physaria 'medicinae'", "white",
    0.0235, 1.5, -0.1, 9, "Physaria didymocarpa subsp. lanata", "white",
    0.02, 1, -0.1, 9, "Physaria acutifolia", "black",
    0.0225, 2.5, -0.1, 21, "Physaria didymocarpa subsp. lyrata", "black",
    0.02, 2, -0.1, 25, "Physaria eburniflora", "white",
    0.0175, 2, -0.1, 26, "Physaria eburniflora", "white",
    0.0175, 0.05, -0.1, 19, "Physaria didymocarpa subsp. didymocarpa", "white"
  ) %>%
    Thesis::repel_haplotype_labels(
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
    -0.005, 0, -0.01, 47,
    -0.005, -1.5, -0.01, 48,
    -0.0065, -1, -0.01, 49,
    -0.0075, 1, 0.01, 50,
    -0.0075, 2, 0.01, 51,
    -0.0065, 0.25, -0.01, 52
  ) %>%
    Thesis::repel_node_labels(
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
      t = 1.5,
      r = 1,
      b = 0.5,
      l = 0.75,
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
    0.015, -0.5, -0.1, 3, "Physaria vitulifera", "black",
    0.014, -1, -0.1, 3, "Physaria acutifolia", "black",
    0.0145, -2.25, 0.1, 8, "Physaria acutifolia", "black",
    0.0175, -2.25, 0.1, 8, "Physaria condensata", "black",
    0.0185, -1.7, 0.1, 8, "Physaria dornii", "black",
    0.021, -1.65, 0.1, 8, "Physaria integrifolia", "white",
    0.023, -1.5, 0.1, 8, "Physaria didymocarpa subsp. lanata", "white",
    0.02465, -1.25, 0.1, 8, "Physaria 'medicinae'", "white",
    0.0175, 1.25, -0.1, 9, "Physaria didymocarpa subsp. lanata", "white",
    0.015, 0.75, -0.1, 9, "Physaria acutifolia", "black",
    0.0135, 2, -0.1, 21, "Physaria didymocarpa subsp. lyrata", "black",
    0.015, 2, -0.1, 25, "Physaria eburniflora", "white",
    0.0125, 2, -0.1, 26, "Physaria eburniflora", "white",
    0.012, 0.05, -0.1, 19, "Physaria didymocarpa subsp. didymocarpa", "white"
  ) %>%
    Thesis::repel_haplotype_labels(
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
    -0.005, 0, -0.01, 47,
    -0.005, -1.5, -0.01, 48,
    -0.0065, -1, -0.01, 49,
    -0.0075, 1, 0.01, 50,
    -0.0075, 2, 0.01, 51,
    -0.0065, 0.25, -0.01, 52
  ) %>%
    Thesis::repel_node_labels(
      node_nudges = .,
      node_labels = tree$nodes,
      initial_ggtree = tree$repel_png,
      label_size = 2
    ) %>%
      rlang::eval_tidy(expr = .)

tree$png <- tree$repel_png +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      override.aes = list(size = 3),
      title.position = "top",
      ncol = 1,
      keyheight = 0.2,
      default.unit = "inch"
    )
  ) +
  ggplot2::theme(
    legend.position = "right",
    legend.text = ggtext::element_markdown(size = 12),
    plot.margin = ggplot2::margin(
      t = 1.5,
      r = 1,
      b = 0.5,
      l = 0.75,
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
      filename = fs::path("inst/figures/ResultsBayes_rITS", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })

