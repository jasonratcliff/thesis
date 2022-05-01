library(thesis)
library(ggplot2)
library(ggtree)
library(cowplot)

# BEAST multi ----

beast <- list()
beast$data <-
  system.file(
    "extdata/BEAST/multi-locus.combined.mcc",
    package = "thesis"
  ) %>%
  thesis::read_tree(tree_file = .)

beast$ggplot <-
  ggtree::ggtree(beast$data, layout = "circular") +
  thesis::beast_posterior() +
  ggtree::geom_tiplab(offset = 0.0005, align = TRUE, size = 3) +
  ggnewscale::new_scale_color() +
  ggplot2::geom_point(
    data = dplyr::filter(beast$data, !is.na(.data$prior_id)),
    mapping = ggplot2::aes(
      color = .data$prior_id,
      shape = .data$prior_id
    ), size = 6, alpha = 0.5
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(beast$data, !is.na(.data$Taxon_a_posteriori)),
    mapping = ggplot2::aes(
      color = .data$Taxon_a_posteriori,
      shape = .data$Taxon_a_posteriori
    ), size = 3
  ) +
  ggrepel::geom_label_repel(
    data = dplyr::filter(beast$data, .data$posterior > 0.5),
    mapping = ggplot2::aes(label = round(.data$posterior, 3)),
    nudge_x = -0.0005,
    nudge_y = -1,
    segment.linetype = 2,
    segment.color = "black",
    segment.curvature = 0.1,
    segment.shape = 0.5,
    size = 3,
    alpha = 0.75
  ) +
  thesis::beast_theme(tree_data = beast$data) +
  ggtree::geom_treescale(y = 48) +
  ggtree::theme_tree() +
  ggplot2::theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.2, 0.3, 0.2, "in")
  )

# png ----

beast$legend_png <-
  cowplot::plot_grid(
    plotlist = list(
      thesis::beast_legend_color(
        tree_data = beast$data,
        ncol = 1
      ),
      thesis::beast_legend_probability(tree_data = beast$data)
    ),
    nrow = 2, ncol = 1, rel_widths = c(0.5, 0.5)
  )

beast$png <-
  cowplot::plot_grid(
    beast$ggplot,
    beast$legend_png,
    nrow = 1, rel_widths = c(0.75, 0.25)
  )

# pdf ----

beast$legend_pdf <-
  cowplot::plot_grid(
    NULL,
    thesis::beast_legend_color(tree_data = beast$data),
    thesis::beast_legend_probability(tree_data = beast$data),
    nrow = 1, ncol = 3, rel_widths = c(0.25, 1, .75)
  )

beast$pdf <-
  cowplot::plot_grid(
    NULL,
    beast$ggplot,
    beast$legend_pdf,
    nrow = 3, rel_heights = c(0.05, 0.65, 0.3)
  )

# plot grids ----

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(beast$png, beast$pdf),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBeastMultiLocus", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
