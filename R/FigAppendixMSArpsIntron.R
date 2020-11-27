library(ThesisPackage)
library(ggplot2)
library(cowplot)

# rps Intron Alignment ----
ggMSArpsIntron <-
  system.file("extdata/Alignments/rps-single.fasta",
              package = "ThesisPackage") %>%
  potentially_informative_sites(
    fasta_file = .,
    seq_name = TRUE
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3)
  )

FigAppendixMSArpsIntron <- plot_grid(ggMSArpsIntron)

ThesisPackage::save_plot(gg_plot = FigAppendixMSArpsIntron)
