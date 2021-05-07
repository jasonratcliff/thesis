library(ThesisPackage)
library(ggplot2)
library(cowplot)

# rITS Alignment ----
ggMSArITS <-
  system.file("extdata/Alignments/rITS-single.fasta",
              package = "ThesisPackage") %>%
  potentially_informative_sites(
    fasta_file = .,
    seq_name = TRUE
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3)
  )

FigAppendixMSArITS <- plot_grid(ggMSArITS)

ThesisPackage::save_plot(gg_plot = FigAppendixMSArITS)
