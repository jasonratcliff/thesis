library(thesis)
library(ggplot2)
library(cowplot)

# ycf1 Intron Alignment ----
ggMSAycf1 <-
  system.file("extdata/Alignments/ycf1-single.fasta",
              package = "thesis") %>%
  potentially_informative_sites(
    fasta_file = .,
    seq_name = TRUE,
    char_width = 0.5
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3)
  )

AppendixMSAycf1 <- plot_grid(ggMSAycf1)

purrr::walk(
  .x = c("png", "pdf"),
  .f = function(ext) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/AppendixMSAycf1", ext = ext),
      plot = AppendixMSAycf1
    )
  })
