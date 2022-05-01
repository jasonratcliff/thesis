library(thesis)
library(cowplot)
library(dplyr)
library(fs)
library(ggplot2)
library(ggtree)
library(ggrepel)

starBeast <-
  purrr::map2(
    .x = fs::path("extdata/BEAST/", paste0("spp-hypothesis-", 1:4), ext = "mcc"),
    .y = 1:4,
    .f = function(mcc, title) {
      treeio::read.beast(
        file = system.file(mcc, package = "thesis")
      ) %>%
        ggtree::fortify() %>%
        dplyr::mutate(
          label = gsub("medicinae", "'medicinae'", x = .data$label)
        ) %>%
        thesis::species_plot(tree_tibble = .) +
        ggplot2::ggtitle(label = paste("Hypothesis", title))
    }) %>%
  stats::setNames(object = ., nm = paste0("hyp", 1:4))

# cowplot Grid ----

ggtree_spp <- list()
ggtree_spp$theme <-
  list(
    ggplot2::theme(
      legend.position = "none"
    )
  )

ggtree_spp$spp <-
  cowplot::plot_grid(
    starBeast$hyp1 + ggtree_spp$theme,
    starBeast$hyp2 + ggtree_spp$theme,
    starBeast$hyp3 + ggtree_spp$theme,
    starBeast$hyp4 + ggtree_spp$theme,
    nrow = 2
  )

ggtree_spp$plot <-
  cowplot::plot_grid(
    ggtree_spp$spp, cowplot::get_legend(starBeast$hyp1),
    ncol = 2, rel_widths = c(0.9, 0.1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    plot = list(ggtree_spp$plot, ggtree_spp$plot),
    width = c(6, 6),
    height = c(8, 4),
    aspect = c(.167, 2.5),
    row = c(1, 2),
    col = c(2, 1)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/ResultsBeastSpecies", ext = ext),
      plot = plot,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = row,
      ncol = col
    )
  })
